use ariadne::{Label, Report, Source};
use std::{
    collections::HashMap,
    error::Error,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    expr::{BinaryOp, ExprKind, ExprView, Expressions, ParseError},
    lexer::{SourceSpan, tokens},
    types::Type,
};

mod atom;
mod expr;
mod lexer;
mod types;

#[derive(Debug)]
pub struct RuntimeError {
    reason: String,
    span: SourceSpan,
}

impl RuntimeError {
    pub fn new(reason: String, span: SourceSpan) -> Self {
        Self { reason, span }
    }

    pub fn type_error(op: &str, type1: Type, type2: Type, span: SourceSpan) -> Self {
        Self::new(
            format!(
                "Operator `{}` not supported for types `{}` and `{}`",
                op, type1, type2
            ),
            span,
        )
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.reason)
    }
}

impl Error for RuntimeError {}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Unit,
    Bool(bool),
    Number(f64),
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Unit => Type::Unit,
            Value::Bool(_) => Type::Bool,
            Value::Number(_) => Type::Number,
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
                "Invalid type for negation".to_string(),
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
        }
    }
}

fn eval(expr: ExprView, cx: &mut Context) -> Result<Value, RuntimeError> {
    match expr.expr() {
        ExprKind::Identifier(name) => cx.get_symbol(name).ok_or_else(|| {
            RuntimeError::new(format!("Undefined symbol {}", name), expr.source_span())
        }),
        ExprKind::Number(value) => Ok(Value::Number(*value)),
        ExprKind::Bool(value) => Ok(Value::Bool(*value)),
        ExprKind::Binary { op, lhs, rhs } => {
            let rhs = eval(expr.with_id(*rhs), cx)?;
            if op == &BinaryOp::Assign {
                let identifier = match expr.with_id(*lhs).expr() {
                    ExprKind::Identifier(id) => id.clone(),
                    _ => {
                        return Err(RuntimeError::new(
                            format!("Expected identifier, found {}", op),
                            expr.source_span(),
                        ));
                    }
                };
                cx.set_symbol(identifier, rhs);
                return Ok(Value::Unit);
            };

            let lhs = eval(expr.with_id(*lhs), cx)?;

            match op {
                BinaryOp::Add => lhs.try_add(&rhs, expr.source_span()),
                BinaryOp::Sub => lhs.try_sub(&rhs, expr.source_span()),
                BinaryOp::Mul => lhs.try_mul(&rhs, expr.source_span()),
                BinaryOp::Div => lhs.try_div(&rhs, expr.source_span()),
                BinaryOp::Eq => Ok(Value::Bool(lhs.eq(&rhs))),
                BinaryOp::NotEq => Ok(Value::Bool(!lhs.eq(&rhs))),
                BinaryOp::Less => lhs.lt(&rhs, expr.source_span()).map(Value::Bool),
                BinaryOp::LessEq => lhs.leq(&rhs, expr.source_span()).map(Value::Bool),
                BinaryOp::Greater => lhs.gt(&rhs, expr.source_span()).map(Value::Bool),
                BinaryOp::GreaterEq => lhs.geq(&rhs, expr.source_span()).map(Value::Bool),
                BinaryOp::Assign => unreachable!(),
            }
        }
        ExprKind::Paren(inner) => eval(expr.with_id(*inner), cx),
        ExprKind::Unary { op, operand } => {
            let inner = eval(expr.with_id(*operand), cx)?;
            match op {
                expr::UnaryOp::Neg => inner.try_neg(expr.source_span()),
                expr::UnaryOp::Not => todo!(),
            }
        }
        ExprKind::Block { children } => {
            for child in children.iter().take(children.len() - 1) {
                eval(expr.with_id(*child), cx)?;
            }
            eval(expr.with_id(*children.last().unwrap()), cx)
        }
    }
}

pub struct Context {
    symbols: HashMap<Rc<str>, Value>,
}

impl Context {
    fn new() -> Self {
        Self {
            symbols: Default::default(),
        }
    }

    fn set_symbol(&mut self, identifier: Rc<str>, value: Value) {
        self.symbols.insert(identifier, value);
    }

    fn get_symbol(&mut self, identifier: &Rc<str>) -> Option<Value> {
        self.symbols.get(identifier).copied()
    }
}

fn format_error(err: &ParseError) -> Report<'_, SourceSpan> {
    let mut builder = Report::build(ariadne::ReportKind::Error, err.span);
    builder = match &err.kind {
        expr::ParseErrorKind::UnexpectedToken { found, expected } => {
            builder.with_message("Unexpected token").with_label(
                Label::new(err.span)
                    .with_message(format!("Found `{}`, expected `{}`", &found, &expected)),
            )
        }
        expr::ParseErrorKind::UnexpectedEOF => builder.with_message("Unexpected end of file"),
    };
    builder.finish()
}

fn main() {
    let source = "a = 2 + -2; b = a + 1 < 10; a + b";
    let tokens = tokens(source).unwrap();
    let mut exprs = Expressions::new();
    let expr = match exprs.parse(&tokens) {
        Ok(expr) => expr,
        Err(err) => {
            let report = format_error(&err);
            report.print(Source::from(source)).unwrap();
            return;
        }
    };
    let mut cx = Context::new();
    let value = match eval(expr, &mut cx) {
        Ok(value) => value,
        Err(err) => {
            Report::build(ariadne::ReportKind::Error, err.span)
                .with_message(&err.reason)
                .finish()
                .print(Source::from(source))
                .unwrap();
            return;
        }
    };
    println!("{:?} -> {:?}", expr, value);
}
