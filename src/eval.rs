use std::{collections::HashMap, error::Error, fmt::Display};

use crate::{
    atom::Atom,
    expr::{BinaryOp, ExprKind, ExprView, UnaryOp},
    lexer::SourceSpan,
    types::Type,
    value::Value,
};

#[derive(Debug)]
pub struct RuntimeError {
    pub title: String,
    pub reason: String,
    pub span: SourceSpan,
}

impl RuntimeError {
    pub fn new(title: impl ToString, reason: String, span: SourceSpan) -> Self {
        Self {
            title: title.to_string(),
            reason,
            span,
        }
    }

    pub fn type_error(op: &str, type1: Type, type2: Type, span: SourceSpan) -> Self {
        Self::new(
            "Type error",
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

pub fn eval(expr: ExprView, cx: &mut EvalContext) -> Result<Value, RuntimeError> {
    match expr.expr() {
        ExprKind::Identifier(atom) => cx.get_symbol(*atom).ok_or_else(|| {
            RuntimeError::new(
                "Undefined symbol",
                format!("Could not find symbol {:?} in the current scope", &expr),
                expr.source_span(),
            )
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
                            "Expected identifier",
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
                UnaryOp::Neg => inner.try_neg(expr.source_span()),
                UnaryOp::Not => inner.try_not(expr.source_span()),
            }
        }
        ExprKind::Block { children } => cx.with_scope(|cx| {
            for child in children.iter().take(children.len() - 1) {
                eval(expr.with_id(*child), cx)?;
            }
            eval(expr.with_id(*children.last().unwrap()), cx)
        }),
        ExprKind::FunctionCall { func, args } => {
            let f = eval(expr.with_id(*func), cx)?;
            let arg_exprs = args;
            let Value::Fn {
                args,
                def,
                captures,
            } = f
            else {
                return Err(RuntimeError::new(
                    "Unexpected",
                    format!("Expected function, found {}", f.get_type()),
                    expr.source_span(),
                ));
            };

            if arg_exprs.len() != args.len() {
                return Err(RuntimeError::new(
                    "Unexpected",
                    format!(
                        "Expected {} arguments, found {}",
                        args.len(),
                        arg_exprs.len()
                    ),
                    expr.source_span(),
                ));
            }
            cx.with_scope(|cx| {
                for (atom, value) in captures.iter() {
                    cx.set_symbol(*atom, value.clone());
                }

                for (atom, value_expr) in args.iter().zip(arg_exprs) {
                    let value = eval(expr.with_id(*value_expr), cx)?;
                    cx.set_symbol(*atom, value);
                }

                eval(expr.with_id(def), cx)
            })
        }
        ExprKind::FunctionDef(def) => {
            let mut captures = HashMap::new();
            for &capture in def.captures.iter() {
                let value = cx.get_symbol(capture).unwrap();
                captures.insert(capture, value);
            }

            Ok(Value::Fn {
                args: def.args.clone(),
                def: def.body,
                captures,
            })
        }
    }
}

#[derive(Clone)]
pub struct EvalContext {
    symbols: HashMap<Atom, Value>,
}

impl EvalContext {
    pub fn new() -> Self {
        Self {
            symbols: Default::default(),
        }
    }

    fn set_symbol(&mut self, identifier: Atom, value: Value) {
        self.symbols.insert(identifier, value);
    }

    fn get_symbol(&mut self, identifier: Atom) -> Option<Value> {
        self.symbols.get(&identifier).cloned()
    }

    fn with_scope<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let mut inner = self.clone();
        f(&mut inner)
    }
}
