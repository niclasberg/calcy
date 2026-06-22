use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Display,
};

use crate::{
    atom::Atom,
    expr::{BinaryOp, Expr, ExprId, ExprKind, Expressions, TypeAnnotation, UnaryOp},
    lexer::SourceSpan,
    types::Type::Never,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum Type {
    /// Bottom type, represents a type that could never be constructed, and which is a subtype of any type
    Never,
    Unit,
    Bool,
    BoolLiteral(bool),
    Number,
    Array(Box<Type>),
    Fn(Vec<Type>, Box<Type>),
    // The set must have at least 2 members
    Enum(BTreeSet<Type>),
    Struct(BTreeMap<String, Type>),
}

impl Type {
    pub fn join(mut it: impl Iterator<Item = Self>) -> Self {
        let Some(first) = it.next() else {
            return Type::Never;
        };
        let Some(second) = it.next() else {
            return first;
        };
        if first.is_subtype_of(&second) {
            return second;
        }
        if second.is_subtype_of(&first) {
            return first;
        }

        let mut values: BTreeSet<_> = it.collect();
        values.insert(first);
        values.insert(second);
        Type::Enum(values)
    }

    pub fn is_subtype_of(&self, other: &Self) -> bool {
        match (self, other) {
            (l, r) if l == r => true,
            (Type::BoolLiteral(_), Type::Bool) => true,
            (Type::BoolLiteral(lhs), Type::BoolLiteral(rhs)) => *lhs == *rhs,
            (Type::Fn(lhs_args, lhs_ret), Type::Fn(rhs_args, rhs_ret)) => {
                lhs_args.len() == rhs_args.len()
                    && lhs_args
                        .iter()
                        .zip(rhs_args.iter())
                        .all(|(lhs_arg, rhs_arg)| rhs_arg.is_subtype_of(lhs_arg))
                    && lhs_ret.is_subtype_of(rhs_ret)
            }
            (_, Type::Never) => true,
            _ => false,
        }
    }

    pub fn intersect(&self, other: &Self) -> Self {
        match (self, other) {
            (Type::Bool, Type::Bool) => Self::Bool,
            (Type::Unit, Type::Unit) => Self::Unit,
            (Type::Number, Type::Number) => Self::Number,
            (Type::Enum(e1), Type::Enum(e2)) => Self::join(e1.intersection(e2).cloned()),
            (Type::Enum(e), Type::Never) | (Type::Never, Type::Enum(e)) => Type::Enum(e.clone()),
            (Type::Enum(e), other) | (other, Type::Enum(e)) => {
                let mut inner = e.clone();
                inner.insert(other.clone());
                Type::Enum(inner)
            }
            (Type::Struct(fields1), Type::Struct(fields2)) => {
                let mut fields = fields1.clone();
                for (name, t2) in fields2.iter() {
                    if let Some(t) = fields.get_mut(name) {
                        *t = t.intersect(t2);
                        if *t == Type::Never {
                            return Type::Never;
                        }
                    } else {
                        fields.insert(name.clone(), t2.clone());
                    }
                }
                Type::Struct(fields)
            }
            _ => Self::Never,
        }
    }
}

impl From<&TypeAnnotation> for Type {
    fn from(value: &TypeAnnotation) -> Self {
        match value {
            TypeAnnotation::Never => Type::Never,
            TypeAnnotation::Unit => Type::Unit,
            TypeAnnotation::Bool => Type::Bool,
            TypeAnnotation::Float => Type::Number,
            TypeAnnotation::Fn(args, ret) => Type::Fn(
                args.iter().map(Self::from).collect(),
                Box::new(ret.as_ref().into()),
            ),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Never => f.write_str("!"),
            Type::Unit => f.write_str("Unit"),
            Type::Bool => f.write_str("Bool"),
            Type::BoolLiteral(value) => Display::fmt(value, f),
            Type::Number => f.write_str("Number"),
            Type::Array(t) => write!(f, "[{}]", &t),
            Type::Enum(alts) => {
                for alt in alts.iter().take(alts.len() - 1) {
                    alt.fmt(f)?;
                    f.write_str(" | ")?;
                }
                alts.last().map(|alt| alt.fmt(f)).unwrap_or(Ok(()))
            }
            Type::Struct(_) => todo!(),
            Type::Fn(args, ret) => {
                write!(f, "fn (")?;
                for arg in args.iter() {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, ") -> {}", &ret)
            }
        }
    }
}

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
    ExpectedFunction { found: Type },
    UnexpectedType { expected: Type, actual: Type },
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
        ExprKind::Number(_) => Ok(Type::Number),
        ExprKind::Bool(value) => Ok(Type::BoolLiteral(*value)),
        ExprKind::Identifier(atom) => cx.symbols.get(atom).cloned().ok_or(TypeError::new(
            TypeErrorKind::UndefinedVariable(*atom),
            expr.span,
        )),
        ExprKind::Array(element_ids) => {
            if element_ids.is_empty() {
                Err(TypeError::new(TypeErrorKind::NeedTypeAnnotation, expr.span))
            } else {
                let element_types = element_ids
                    .iter()
                    .map(|e| infer(cx, expressions, *e))
                    .collect::<Result<Vec<Type>, TypeError>>()?;
                Ok(Type::join(element_types.into_iter()))
            }
        }
        ExprKind::FunctionCall { func, args } => {
            let func_type = infer(cx, expressions, *func)?;
            let Type::Fn(arg_types, ret_type) = func_type else {
                return Err(TypeError::new(
                    TypeErrorKind::ExpectedFunction { found: func_type },
                    expressions[*func].span,
                ));
            };
            if args.len() != arg_types.len() {
                panic!("Invalid number of arguments");
            }

            for (arg, arg_type) in args.iter().zip(arg_types.iter()) {
                check(cx, expressions, *arg, arg_type)?;
            }

            Ok(ret_type.as_ref().clone())
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
            Ok(Type::Unit)
        }
        ExprKind::IfThenElse { cond, lhs, rhs } => {
            check(cx, expressions, *cond, &Type::Bool)?;
            let lhs_type = infer(cx, expressions, *lhs)?;
            let rhs_type = infer(cx, expressions, *rhs)?;
            Ok(Type::join([lhs_type, rhs_type].into_iter()))
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
                    return Err(TypeError::new(TypeErrorKind::NeedTypeAnnotation, expr.span));
                }
            }
            let ret_type = if let Some(ret_type_annotation) = ret_type {
                let ret_type = ret_type_annotation.into();
                check(cx, expressions, *body, &ret_type)?;
                ret_type
            } else {
                infer(cx, expressions, *body)?
            };
            Ok(Type::Fn(arg_types, Box::new(ret_type)))
        }),
        ExprKind::Unary { op, operand } => match op {
            UnaryOp::Neg => check(cx, expressions, *operand, &Type::Number).map(|_| Type::Number),
            UnaryOp::Not => check(cx, expressions, *operand, &Type::Bool).map(|_| Type::Bool),
        },
        ExprKind::Binary { lhs, op, rhs } => match op {
            BinaryOp::Assign => {
                let lhs_expr = &expressions[*lhs];
                let ExprKind::Identifier(id) = lhs_expr.kind else {
                    return Err(TypeError::new(
                        TypeErrorKind::ExpectedIdentifier,
                        lhs_expr.span,
                    ));
                };
                let lhs_type = cx.symbols.get(&id).cloned().ok_or(TypeError::new(
                    TypeErrorKind::UndefinedVariable(id),
                    expr.span,
                ))?;
                check(cx, expressions, *rhs, &lhs_type)?;
                Ok(lhs_type)
            }
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                check(cx, expressions, *lhs, &Type::Number)?;
                check(cx, expressions, *rhs, &Type::Number)?;
                Ok(Type::Number)
            }
            BinaryOp::Less | BinaryOp::LessEq | BinaryOp::Greater | BinaryOp::GreaterEq => {
                check(cx, expressions, *lhs, &Type::Number)?;
                check(cx, expressions, *rhs, &Type::Number)?;
                Ok(Type::Bool)
            }
            BinaryOp::Eq | BinaryOp::NotEq => {
                let lhs_type = infer(cx, expressions, expr_id)?;
                let rhs_type = infer(cx, expressions, expr_id)?;
                if lhs_type.is_subtype_of(&rhs_type) || rhs_type.is_subtype_of(&lhs_type) {
                    Ok(Type::Bool)
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
                if child_type == Type::Never {
                    return Ok(Never);
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
        (ExprKind::Array(value_exprs), Type::Array(value_type)) => {
            for value_expr in value_exprs {
                check(cx, expressions, *value_expr, &value_type)?;
            }
            Ok(())
        }
        (ExprKind::FunctionDef { args, body, .. }, Type::Fn(arg_types, ret_type)) => {
            if args.len() != arg_types.len() {
                panic!("Invalid number of arguments");
            }
            for (arg, arg_type) in args.iter().zip(arg_types.iter()) {
                if let Some(type_annotation) = &arg.type_annotation {
                    todo!()
                }
            }
            let ret = infer(cx, expressions, *body)?;
            if !ret_type.is_subtype_of(&ret) {
                panic!("Invalid return type");
            }
            Ok(())
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
