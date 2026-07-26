use std::collections::{BTreeMap, HashMap};

use crate::{
    atom::Atom,
    expr::{BinaryOp, ExprId, ExprKind, Expressions, UnaryOp},
    lexer::SourceSpan,
    types::{Type, TypeBuilder},
};

pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: SourceSpan,
}

impl TypeError {
    pub fn new(kind: TypeErrorKind, span: SourceSpan) -> Self {
        Self { kind, span }
    }
}

pub enum TypeErrorKind {
    UndefinedVariable(Atom),
    NeedTypeAnnotation,
    ExpectedIdentifier,
    ExpectedArray { found: Type },
    ExpectedFunction { found: Type },
    UnexpectedType { expected: Type, actual: Type },
    ArgumentCountMismatch { expected: usize, found: usize },
}

pub struct TypeContext {
    symbols: HashMap<Atom, Type>,
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            symbols: Default::default(),
        }
    }

    fn with_scope<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let symbols = self.symbols.clone();
        let result = f(self);
        self.symbols = symbols;
        result
    }
}

pub fn infer(
    cx: &mut TypeContext,
    expressions: &Expressions,
    expr_id: ExprId,
) -> Result<Type, TypeError> {
    let expr = &expressions[expr_id];
    match &expr.kind {
        ExprKind::Number(_) => Ok(Type::FLOAT),
        ExprKind::Bool(value) => Ok(Type::bool_lit(*value)),
        ExprKind::Identifier(atom) => cx.symbols.get(atom).cloned().ok_or(TypeError::new(
            TypeErrorKind::UndefinedVariable(*atom),
            expr.span,
        )),
        ExprKind::Array(element_ids) => {
            if element_ids.is_empty() {
                Err(TypeError::new(TypeErrorKind::NeedTypeAnnotation, expr.span))
            } else {
                let mut type_builder = TypeBuilder::new(Type::Never);
                for element in element_ids.iter() {
                    let elem_type = infer(cx, expressions, element.expr_id)?;
                    if element.flatten {
                        let Type::Array(array_type) = elem_type else {
                            return Err(TypeError::new(
                                TypeErrorKind::ExpectedArray { found: elem_type },
                                element.span,
                            ));
                        };
                        type_builder = type_builder.union(array_type.elem.as_ref().clone());
                    } else {
                        type_builder = type_builder.union(elem_type);
                    }
                }

                Ok(Type::array(type_builder.finish()))
            }
        }
        ExprKind::Record(elems) => {
            let mut elements = BTreeMap::new();
            for elem in elems.iter() {
                let elem_type = infer(cx, expressions, elem.value)?;
                let existing = elements.insert(elem.name, elem_type);
                if existing.is_some() {
                    panic!("Duplicate struct element");
                }
            }
            Ok(Type::record(elements))
        }
        ExprKind::FunctionCall { func, args } => {
            let func_type = infer(cx, expressions, *func)?;
            let Type::Fn(fn_type) = func_type else {
                return Err(TypeError::new(
                    TypeErrorKind::ExpectedFunction { found: func_type },
                    expressions[*func].span,
                ));
            };

            if args.len() != fn_type.args.len() {
                return Err(TypeError::new(
                    TypeErrorKind::ArgumentCountMismatch {
                        expected: fn_type.args.len(),
                        found: args.len(),
                    },
                    expressions[*func].span,
                ));
            }

            for (arg, arg_type) in args.iter().zip(fn_type.args.iter()) {
                check(cx, expressions, *arg, arg_type)?;
            }

            Ok(fn_type.ret.as_ref().clone())
        }
        ExprKind::Index { index_expr, source } => {
            check(cx, expressions, *index_expr, &Type::FLOAT)?;
            let arr_type = infer(cx, expressions, *source)?;
            let Type::Array(arr) = arr_type else {
                return Err(TypeError::new(
                    TypeErrorKind::ExpectedArray { found: arr_type },
                    expressions[*source].span,
                ));
            };
            Ok(arr.elem.as_ref().clone())
        }
        ExprKind::Let {
            id,
            value,
            type_annotation,
        } => {
            let t = if let Some(expected_type) = type_annotation {
                let t = expected_type.into();
                check(cx, expressions, *value, &t)?;
                t
            } else {
                infer(cx, expressions, *value)?
            };
            cx.symbols.insert(*id, t);
            Ok(Type::UNIT)
        }
        ExprKind::IfThenElse { cond, lhs, rhs } => {
            check(cx, expressions, *cond, &Type::BOOL)?;
            let lhs_type = infer(cx, expressions, *lhs)?;
            let rhs_type = infer(cx, expressions, *rhs)?;
            Ok(Type::join([lhs_type, rhs_type]))
        }
        ExprKind::Match { value_expr } => {
            let value_type = infer(cx, expressions, *value_expr)?;

            todo!()
        }
        ExprKind::FunctionDef {
            args,
            body,
            ret_type,
            ..
        } => cx.with_scope(|cx| {
            let mut arg_types = Vec::new();
            for arg in args.iter() {
                if let Some(type_annotation) = &arg.type_annotation {
                    let t: Type = type_annotation.into();
                    cx.symbols.insert(arg.id, t.clone());
                    arg_types.push(t);
                } else {
                    // If we start doing local unification, we could introduce a type variable here...
                    return Err(TypeError::new(TypeErrorKind::NeedTypeAnnotation, arg.span));
                }
            }
            let ret_type = if let Some(ret_type_annotation) = ret_type {
                let ret_type = ret_type_annotation.into();
                check(cx, expressions, *body, &ret_type)?;
                ret_type
            } else {
                infer(cx, expressions, *body)?
            };
            Ok(Type::func(arg_types, ret_type))
        }),
        ExprKind::Unary { op, operand } => match op {
            UnaryOp::Neg => check(cx, expressions, *operand, &Type::FLOAT).map(|_| Type::FLOAT),
            UnaryOp::Not => check(cx, expressions, *operand, &Type::BOOL).map(|_| Type::BOOL),
        },
        ExprKind::Binary { lhs, op, rhs } => match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                check(cx, expressions, *lhs, &Type::FLOAT)?;
                check(cx, expressions, *rhs, &Type::FLOAT)?;
                Ok(Type::FLOAT)
            }
            BinaryOp::Less | BinaryOp::LessEq | BinaryOp::Greater | BinaryOp::GreaterEq => {
                check(cx, expressions, *lhs, &Type::FLOAT)?;
                check(cx, expressions, *rhs, &Type::FLOAT)?;
                Ok(Type::BOOL)
            }
            BinaryOp::Eq | BinaryOp::NotEq => {
                let lhs_type = infer(cx, expressions, expr_id)?;
                let rhs_type = infer(cx, expressions, expr_id)?;
                if lhs_type.is_subtype_of(&rhs_type) || rhs_type.is_subtype_of(&lhs_type) {
                    Ok(Type::BOOL)
                } else {
                    panic!("Types not matching!");
                }
            }
        },
        ExprKind::Block { children } => {
            let mut iter = children.iter();
            let last = iter.next_back().expect("Blocks should never be empty");
            for child_id in iter {
                let child_type = infer(cx, expressions, *child_id)?;
                if child_type == Type::NEVER {
                    return Ok(Type::NEVER);
                }
            }
            infer(cx, expressions, *last)
        }
    }
}

pub fn check(
    cx: &mut TypeContext,
    expressions: &Expressions,
    expr_id: ExprId,
    expected: &Type,
) -> Result<(), TypeError> {
    let expr = &expressions[expr_id];
    match (&expr.kind, expected) {
        (ExprKind::Array(value_exprs), Type::Array(arr)) => {
            for e in value_exprs {
                let expected_type = if e.flatten {
                    expected
                } else {
                    arr.elem.as_ref()
                };
                check(cx, expressions, e.expr_id, expected_type)?;
            }
            Ok(())
        }
        (ExprKind::FunctionDef { args, body, .. }, Type::Fn(f)) => {
            if args.len() != f.args.len() {
                return Err(TypeError::new(
                    TypeErrorKind::ArgumentCountMismatch {
                        expected: f.args.len(),
                        found: args.len(),
                    },
                    expr.span,
                ));
            }

            cx.with_scope(|cx| {
                for (arg, arg_type) in args.iter().zip(f.args.iter()) {
                    if let Some(type_annotation) = &arg.type_annotation {
                        let t: Type = type_annotation.into();
                        if !t.is_subtype_of(arg_type) {
                            return Err(TypeError::new(
                                TypeErrorKind::UnexpectedType {
                                    expected: arg_type.clone(),
                                    actual: t,
                                },
                                expr.span,
                            ));
                        }
                    }
                    cx.symbols.insert(arg.id, arg_type.clone());
                }
                check(cx, expressions, *body, &f.ret)
            })
        }
        _ => {
            let t = infer(cx, expressions, expr_id)?;
            if !t.is_subtype_of(expected) {
                Err(TypeError::new(
                    TypeErrorKind::UnexpectedType {
                        expected: expected.clone(),
                        actual: t,
                    },
                    expr.span,
                ))
            } else {
                Ok(())
            }
        }
    }
}
