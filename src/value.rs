use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use crate::{atom::Atom, eval::RuntimeError, expr::ExprId, lexer::SourceSpan};

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Unit,
    Bool,
    Number,
    Array,
    Fn,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => f.write_str("Unit"),
            Self::Bool => f.write_str("Bool"),
            Self::Number => f.write_str("Number"),
            Self::Array => f.write_str("Array"),
            Self::Fn => f.write_str("fn"),
        }
    }
}

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
    Array(Vec<Value>),
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        match self {
            Value::Unit => ValueType::Unit,
            Value::Bool(_) => ValueType::Bool,
            Value::Number(_) => ValueType::Number,
            Value::Fn { .. } => ValueType::Fn,
            Value::Array(..) => ValueType::Array,
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
            Value::Array(values) => {
                f.write_char('[')?;
                let mut first = true;
                for value in values.iter() {
                    if !first {
                        write!(f, ", {}", value)?;
                    } else {
                        write!(f, "{}", value)?;
                    }
                    first = false;
                }
                f.write_char(']')?;
                Ok(())
            }
        }
    }
}
