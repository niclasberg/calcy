use std::{collections::HashMap, error::Error, fmt::Display};

use crate::{
    atom::Atom,
    expr::{BinaryOp, ExprId, ExprKind, Expressions, UnaryOp},
    lexer::SourceSpan,
    types::Type,
    value::{Value, ValueType},
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

    pub fn type_error(op: &str, type1: ValueType, type2: ValueType, span: SourceSpan) -> Self {
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

pub fn eval(
    id: ExprId,
    expressions: &Expressions,
    cx: &mut EvalContext,
) -> Result<Value, RuntimeError> {
    let expr = &expressions[id];
    match &expr.kind {
        ExprKind::Identifier(atom) => cx.get_symbol(*atom).ok_or_else(|| {
            RuntimeError::new(
                "Undefined symbol",
                format!("Could not find symbol {:?} in the current scope", &expr),
                expr.span,
            )
        }),
        ExprKind::Number(value) => Ok(Value::Number(*value)),
        ExprKind::Bool(value) => Ok(Value::Bool(*value)),
        ExprKind::Array(value_exprs) => {
            let mut values = Vec::with_capacity(value_exprs.len());
            for value_expr in value_exprs.iter() {
                values.push(eval(*value_expr, expressions, cx)?);
            }
            Ok(Value::Array(values))
        }
        ExprKind::Binary { op, lhs, rhs } => {
            let rhs = eval(*rhs, expressions, cx)?;
            if op == &BinaryOp::Assign {
                let id_expr = &expressions[*lhs];
                let identifier = match &id_expr.kind {
                    ExprKind::Identifier(id) => *id,
                    _ => {
                        return Err(RuntimeError::new(
                            "Expected identifier",
                            format!("Expected identifier, found {}", op),
                            id_expr.span,
                        ));
                    }
                };

                if !cx.has_symbol(identifier) {
                    return Err(RuntimeError::new(
                        "Undefined symbol",
                        format!("Symbol {} not defined in the current scope", "asdf"),
                        id_expr.span,
                    ));
                }

                cx.set_symbol(identifier, rhs);
                return Ok(Value::Unit);
            };

            let lhs = eval(*lhs, expressions, cx)?;

            match op {
                BinaryOp::Add => lhs.try_add(&rhs, expr.span),
                BinaryOp::Sub => lhs.try_sub(&rhs, expr.span),
                BinaryOp::Mul => lhs.try_mul(&rhs, expr.span),
                BinaryOp::Div => lhs.try_div(&rhs, expr.span),
                BinaryOp::Eq => Ok(Value::Bool(lhs.eq(&rhs))),
                BinaryOp::NotEq => Ok(Value::Bool(!lhs.eq(&rhs))),
                BinaryOp::Less => lhs.lt(&rhs, expr.span).map(Value::Bool),
                BinaryOp::LessEq => lhs.leq(&rhs, expr.span).map(Value::Bool),
                BinaryOp::Greater => lhs.gt(&rhs, expr.span).map(Value::Bool),
                BinaryOp::GreaterEq => lhs.geq(&rhs, expr.span).map(Value::Bool),
                BinaryOp::Assign => unreachable!(),
            }
        }
        ExprKind::Unary { op, operand } => {
            let inner = eval(*operand, expressions, cx)?;
            match op {
                UnaryOp::Neg => inner.try_neg(expr.span),
                UnaryOp::Not => inner.try_not(expr.span),
            }
        }
        ExprKind::Block { children } => cx.with_scope(|cx| {
            for child in children.iter().take(children.len() - 1) {
                eval(*child, expressions, cx)?;
            }
            eval(*children.last().unwrap(), expressions, cx)
        }),
        ExprKind::Let { id, value, .. } => {
            let value = eval(*value, expressions, cx)?;
            cx.set_symbol(*id, value);
            Ok(Value::Unit)
        }
        ExprKind::FunctionCall { func, args } => {
            let f = eval(*func, expressions, cx)?;
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
                    expr.span,
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
                    expr.span,
                ));
            }
            cx.with_scope(|cx| {
                for (atom, value) in captures.iter() {
                    cx.set_symbol(*atom, value.clone());
                }

                for (atom, value_expr) in args.iter().zip(arg_exprs) {
                    let value = eval(*value_expr, expressions, cx)?;
                    cx.set_symbol(*atom, value);
                }

                eval(def, expressions, cx)
            })
        }
        ExprKind::FunctionDef {
            captures,
            args,
            body,
            ..
        } => {
            let mut captured_values = HashMap::new();
            for &capture in captures.iter() {
                let value = cx.get_symbol(capture).unwrap();
                captured_values.insert(capture, value);
            }

            Ok(Value::Fn {
                args: args.iter().map(|a| a.id).collect(),
                def: *body,
                captures: captured_values,
            })
        }
        ExprKind::IfThenElse { cond, lhs, rhs } => {
            let cond_value = eval(*cond, expressions, cx)?;
            let Value::Bool(cond_value) = cond_value else {
                return Err(RuntimeError::type_error(
                    "if",
                    cond_value.get_type(),
                    ValueType::Bool,
                    expressions[*cond].span,
                ));
            };
            if cond_value {
                eval(*lhs, expressions, cx)
            } else {
                eval(*rhs, expressions, cx)
            }
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

    fn has_symbol(&mut self, identifier: Atom) -> bool {
        self.symbols.contains_key(&identifier)
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
