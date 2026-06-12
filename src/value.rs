use std::{collections::HashMap, fmt::Display};

use crate::{atom::Atom, eval::RuntimeError, expr::ExprId, lexer::SourceSpan, types::Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit,
    Bool(bool),
    Number(f64),
    Fn {
        args: Vec<Atom>,
        captures: HashMap<Atom, Value>,
        def: ExprId,
    },
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Unit => Type::Unit,
            Value::Bool(_) => Type::Bool,
            Value::Number(_) => Type::Number,
            Value::Fn { .. } => Type::Fn,
        }
    }

    pub fn lt(&self, other: &Self, span: SourceSpan) -> Result<bool, RuntimeError> {
        match (self, other) {
            (Value::Unit, Value::Unit) => Ok(false),
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(lhs < rhs),
            (Value::Number(lhs), Value::Number(rhs)) => Ok(lhs < rhs),
            _ => Err(RuntimeError::type_error(
                "<",
                self.get_type(),
                other.get_type(),
                span,
            )),
        }
    }

    pub fn leq(&self, other: &Self, span: SourceSpan) -> Result<bool, RuntimeError> {
        match (self, other) {
            (Value::Unit, Value::Unit) => Ok(true),
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(lhs <= rhs),
            (Value::Number(lhs), Value::Number(rhs)) => Ok(lhs <= rhs),
            _ => Err(RuntimeError::type_error(
                "<=",
                self.get_type(),
                other.get_type(),
                span,
            )),
        }
    }

    pub fn gt(&self, other: &Self, span: SourceSpan) -> Result<bool, RuntimeError> {
        match (self, other) {
            (Value::Unit, Value::Unit) => Ok(false),
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(lhs > rhs),
            (Value::Number(lhs), Value::Number(rhs)) => Ok(lhs > rhs),
            _ => Err(RuntimeError::type_error(
                ">",
                self.get_type(),
                other.get_type(),
                span,
            )),
        }
    }

    pub fn geq(&self, other: &Self, span: SourceSpan) -> Result<bool, RuntimeError> {
        match (self, other) {
            (Value::Unit, Value::Unit) => Ok(true),
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(lhs >= rhs),
            (Value::Number(lhs), Value::Number(rhs)) => Ok(lhs >= rhs),
            _ => Err(RuntimeError::type_error(
                ">=",
                self.get_type(),
                other.get_type(),
                span,
            )),
        }
    }

    pub fn try_add(&self, other: &Self, span: SourceSpan) -> Result<Self, RuntimeError> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
            _ => Err(RuntimeError::type_error(
                "+",
                self.get_type(),
                other.get_type(),
                span,
            )),
        }
    }

    pub fn try_sub(&self, other: &Self, span: SourceSpan) -> Result<Self, RuntimeError> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs - rhs)),
            _ => Err(RuntimeError::type_error(
                "-",
                self.get_type(),
                other.get_type(),
                span,
            )),
        }
    }

    pub fn try_mul(&self, other: &Self, span: SourceSpan) -> Result<Self, RuntimeError> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs * rhs)),
            _ => Err(RuntimeError::type_error(
                "*",
                self.get_type(),
                other.get_type(),
                span,
            )),
        }
    }

    pub fn try_div(&self, other: &Self, span: SourceSpan) -> Result<Self, RuntimeError> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs / rhs)),
            _ => Err(RuntimeError::type_error(
                "/",
                self.get_type(),
                other.get_type(),
                span,
            )),
        }
    }

    pub fn try_neg(&self, span: SourceSpan) -> Result<Self, RuntimeError> {
        match self {
            Value::Number(value) => Ok(Value::Number(-value)),
            _ => Err(RuntimeError::new(
                "Type error",
                "Invalid type for negation".to_string(),
                span,
            )),
        }
    }

    pub fn try_not(&self, span: SourceSpan) -> Result<Self, RuntimeError> {
        match self {
            Value::Bool(value) => Ok(Value::Bool(!value)),
            _ => Err(RuntimeError::new(
                "Type error",
                "Invalid type for boolean not".to_string(),
                span,
            )),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => f.write_str("Unit"),
            Value::Bool(value) => f.write_str(if *value { "true" } else { "false" }),
            Value::Number(value) => Display::fmt(value, f),
            Value::Fn { .. } => f.write_str("fn"),
        }
    }
}
