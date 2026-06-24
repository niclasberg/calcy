use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Display,
};

use crate::{
    atom::Atom,
    expr::{BinaryOp, ExprId, ExprKind, Expressions, TypeAnnotation, UnaryOp},
    lexer::SourceSpan,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum Type {
    Simple(SimpleType),
    // The set must have at least 2 members
    Enum(BTreeSet<SimpleType>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum SimpleType {
    /// Bottom type, represents a type that could never be constructed, and which is a subtype of any type
    Never,
    Unit,
    Bool,
    BoolLiteral(bool),
    Float,
    Array(Box<Type>),
    Fn(Vec<Type>, Box<Type>),
    Struct(BTreeMap<String, Type>),
}

impl Type {
    pub const NEVER: Self = Self::Simple(SimpleType::Never);
    pub const UNIT: Self = Self::Simple(SimpleType::Unit);
    pub const BOOL: Self = Self::Simple(SimpleType::Bool);
    pub const FLOAT: Self = Self::Simple(SimpleType::Float);
    pub const fn bool_lit(value: bool) -> Self {
        Self::Simple(SimpleType::BoolLiteral(value))
    }

    pub fn join(it: impl Iterator<Item = Self>) -> Self {
        let mut types = BTreeSet::new();
        for t in it {
            match t {
                Type::Simple(simple_type) => {
                    if simple_type != SimpleType::Never {
                        types.insert(simple_type);
                    }
                }
                Type::Enum(btree_set) => {
                    types.extend(btree_set.into_iter().filter(|s| *s != SimpleType::Never))
                }
            }
        }
        if types.is_empty() {
            Type::Simple(SimpleType::Never)
        } else if types.len() == 1 {
            Type::Simple(types.pop_first().unwrap())
        } else {
            Type::Enum(types)
        }
    }

    pub fn is_subtype_of(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Simple(lhs), Type::Simple(rhs)) => lhs.is_subtype_of(rhs),
            (Type::Simple(s), Type::Enum(alts)) => alts.iter().any(|alt| s.is_subtype_of(alt)),
            (Type::Enum(alts), Type::Simple(s)) => alts.iter().all(|alt| alt.is_subtype_of(s)),
            (Type::Enum(alts1), Type::Enum(alts2)) => alts1
                .iter()
                .all(|alt1| alts2.iter().any(|alt2| alt1.is_subtype_of(alt2))),
        }
    }

    pub fn widen_literals(self) -> Self {
        match self {
            Self::Simple(s) => Self::Simple(s.widen_literals()),
            s => s,
        }
    }

    /*pub fn intersect(&self, other: &Self) -> Self {
        match (self, other) {
            (Type::Bool, Type::Bool) => Self::Bool,
            (Type::Unit, Type::Unit) => Self::Unit,
            (Type::Float, Type::Float) => Self::Float,
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
    }*/
}

impl SimpleType {
    pub fn widen_literals(self) -> Self {
        match self {
            Self::BoolLiteral(_) => Self::Bool,
            s => s,
        }
    }

    pub fn is_subtype_of(&self, other: &Self) -> bool {
        match (self, other) {
            (l, r) if l == r => true,
            (Self::BoolLiteral(_), Self::Bool) => true,
            (Self::BoolLiteral(lhs), Self::BoolLiteral(rhs)) => *lhs == *rhs,
            (Self::Fn(lhs_args, lhs_ret), Self::Fn(rhs_args, rhs_ret)) => {
                lhs_args.len() == rhs_args.len()
                    && lhs_args
                        .iter()
                        .zip(rhs_args.iter())
                        .all(|(lhs_arg, rhs_arg)| rhs_arg.is_subtype_of(lhs_arg))
                    && lhs_ret.is_subtype_of(rhs_ret)
            }
            // Right now, arrays are invariant, I think we want to support assignment as well as getters.
            // Making them immutable would allow us to make them covariant
            // Right now we always widen the literal types in let and array expressions, so we don't have to
            // think about it.
            (Self::Array(lhs_elem), Self::Array(rhs_elem)) => lhs_elem == rhs_elem,
            (_, Self::Never) => true,
            _ => false,
        }
    }
}

impl From<&TypeAnnotation> for Type {
    fn from(value: &TypeAnnotation) -> Self {
        match value {
            TypeAnnotation::Never => Type::Simple(SimpleType::Never),
            TypeAnnotation::Unit => Type::Simple(SimpleType::Unit),
            TypeAnnotation::Bool => Type::Simple(SimpleType::Bool),
            TypeAnnotation::Float => Type::Simple(SimpleType::Float),
            TypeAnnotation::Enum(alts) => Type::Enum(
                alts.iter()
                    .map(|alt| {
                        let a: Type = alt.into();
                        let Type::Simple(s) = a else { unreachable!() };
                        s
                    })
                    .collect(),
            ),
            TypeAnnotation::Array(elem) => {
                Type::Simple(SimpleType::Array(Box::new(elem.as_ref().into())))
            }
            TypeAnnotation::Fn(args, ret) => Type::Simple(SimpleType::Fn(
                args.iter().map(Self::from).collect(),
                Box::new(ret.as_ref().into()),
            )),
        }
    }
}

impl Display for SimpleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Never => f.write_str("!"),
            Self::Unit => f.write_str("Unit"),
            Self::Bool => f.write_str("Bool"),
            Self::BoolLiteral(value) => Display::fmt(value, f),
            Self::Float => f.write_str("Float"),
            Self::Array(t) => write!(f, "[{}]", &t),
            Self::Struct(_) => todo!(),
            Self::Fn(args, ret) => {
                write!(f, "(")?;
                for arg in args.iter() {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, ") => {}", &ret)
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Simple(s) => Display::fmt(s, f),
            Type::Enum(alts) => {
                for alt in alts.iter().take(alts.len() - 1) {
                    alt.fmt(f)?;
                    f.write_str(" | ")?;
                }
                alts.last().map(|alt| alt.fmt(f)).unwrap_or(Ok(()))
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
                let element_types = element_ids
                    .iter()
                    .map(|e| {
                        let elem_type = infer(cx, expressions, e.expr_id)?;
                        if e.flatten {
                            let Type::Simple(SimpleType::Array(inner_type)) = elem_type else {
                                return Err(TypeError::new(
                                    TypeErrorKind::ExpectedArray { found: elem_type },
                                    e.span,
                                ));
                            };
                            Ok(inner_type.as_ref().clone())
                        } else {
                            Ok(elem_type)
                        }
                    })
                    .collect::<Result<Vec<Type>, TypeError>>()?;
                Ok(Type::Simple(SimpleType::Array(Box::new(Type::join(
                    element_types.into_iter().map(Type::widen_literals),
                )))))
            }
        }
        ExprKind::FunctionCall { func, args } => {
            let func_type = infer(cx, expressions, *func)?;
            let Type::Simple(SimpleType::Fn(arg_types, ret_type)) = func_type else {
                return Err(TypeError::new(
                    TypeErrorKind::ExpectedFunction { found: func_type },
                    expressions[*func].span,
                ));
            };

            if args.len() != arg_types.len() {
                return Err(TypeError::new(
                    TypeErrorKind::ArgumentCountMismatch {
                        expected: arg_types.len(),
                        found: args.len(),
                    },
                    expressions[*func].span,
                ));
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
            cx.symbols.insert(*id, t.widen_literals());
            Ok(Type::UNIT)
        }
        ExprKind::IfThenElse { cond, lhs, rhs } => {
            check(cx, expressions, *cond, &Type::BOOL)?;
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
            Ok(Type::Simple(SimpleType::Fn(arg_types, Box::new(ret_type))))
        }),
        ExprKind::Unary { op, operand } => match op {
            UnaryOp::Neg => check(cx, expressions, *operand, &Type::FLOAT).map(|_| Type::FLOAT),
            UnaryOp::Not => check(cx, expressions, *operand, &Type::BOOL).map(|_| Type::BOOL),
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
        (ExprKind::Array(value_exprs), Type::Simple(SimpleType::Array(value_type))) => {
            for e in value_exprs {
                let expected_type = if e.flatten {
                    expected
                } else {
                    value_type.as_ref()
                };
                check(cx, expressions, e.expr_id, expected_type)?;
            }
            Ok(())
        }
        (
            ExprKind::FunctionDef { args, body, .. },
            Type::Simple(SimpleType::Fn(arg_types, ret_type)),
        ) => {
            if args.len() != arg_types.len() {
                return Err(TypeError::new(
                    TypeErrorKind::ArgumentCountMismatch {
                        expected: arg_types.len(),
                        found: args.len(),
                    },
                    expr.span,
                ));
            }

            cx.with_scope(|cx| {
                for (arg, arg_type) in args.iter().zip(arg_types.iter()) {
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
                check(cx, expressions, *body, ret_type)
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
