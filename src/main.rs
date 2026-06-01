use slotmap::{SlotMap, new_key_type};
use std::{
    collections::HashMap,
    error::Error,
    fmt::{Debug, Display},
    rc::Rc,
};
use winnow::{
    ModalResult, Parser, Stateful,
    combinator::{Infix, expression},
    error::{ContextError, ErrMode},
    stream::TokenSlice,
    token::any,
};

use crate::{
    expr::BinaryOp,
    lexer::{Keyword, Oper, Token, TokenKind, Tokens, tokens},
};

mod expr;
mod lexer;

new_key_type! {
    pub struct NodeId;
}

#[derive(Debug)]
pub struct RuntimeError {
    reason: String,
}

impl RuntimeError {
    pub fn new(reason: String) -> Self {
        Self { reason }
    }

    pub fn type_error(op: &str, type1: Type, type2: Type) -> Self {
        Self::new(format!(
            "Operator `{}` not supported for types `{}` and `{}`",
            op, type1, type2
        ))
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.reason)
    }
}

impl Error for RuntimeError {}

#[derive(Debug)]
pub enum Type {
    Unit,
    Bool,
    Number,
}

impl Type {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Type::Unit => "Unit",
            Type::Bool => "Bool",
            Type::Number => "Number",
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

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

    pub fn lt(&self, other: &Self) -> Result<bool, RuntimeError> {
        match (self, other) {
            (Value::Unit, Value::Unit) => Ok(false),
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(lhs < rhs),
            (Value::Number(lhs), Value::Number(rhs)) => Ok(lhs < rhs),
            _ => Err(RuntimeError::type_error(
                "<",
                self.get_type(),
                other.get_type(),
            )),
        }
    }

    pub fn leq(&self, other: &Self) -> Result<bool, RuntimeError> {
        match (self, other) {
            (Value::Unit, Value::Unit) => Ok(true),
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(lhs <= rhs),
            (Value::Number(lhs), Value::Number(rhs)) => Ok(lhs <= rhs),
            _ => Err(RuntimeError::type_error(
                "<=",
                self.get_type(),
                other.get_type(),
            )),
        }
    }

    pub fn gt(&self, other: &Self) -> Result<bool, RuntimeError> {
        match (self, other) {
            (Value::Unit, Value::Unit) => Ok(false),
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(lhs > rhs),
            (Value::Number(lhs), Value::Number(rhs)) => Ok(lhs > rhs),
            _ => Err(RuntimeError::type_error(
                ">",
                self.get_type(),
                other.get_type(),
            )),
        }
    }

    pub fn geq(&self, other: &Self) -> Result<bool, RuntimeError> {
        match (self, other) {
            (Value::Unit, Value::Unit) => Ok(true),
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(lhs >= rhs),
            (Value::Number(lhs), Value::Number(rhs)) => Ok(lhs >= rhs),
            _ => Err(RuntimeError::type_error(
                ">=",
                self.get_type(),
                other.get_type(),
            )),
        }
    }

    pub fn try_add(&self, other: &Self) -> Result<Self, RuntimeError> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs + rhs)),
            _ => Err(RuntimeError::type_error(
                "+",
                self.get_type(),
                other.get_type(),
            )),
        }
    }

    pub fn try_sub(&self, other: &Self) -> Result<Self, RuntimeError> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs - rhs)),
            _ => Err(RuntimeError::type_error(
                "-",
                self.get_type(),
                other.get_type(),
            )),
        }
    }

    pub fn try_mul(&self, other: &Self) -> Result<Self, RuntimeError> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs * rhs)),
            _ => Err(RuntimeError::type_error(
                "*",
                self.get_type(),
                other.get_type(),
            )),
        }
    }

    pub fn try_div(&self, other: &Self) -> Result<Self, RuntimeError> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => Ok(Value::Number(lhs / rhs)),
            _ => Err(RuntimeError::type_error(
                "/",
                self.get_type(),
                other.get_type(),
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

#[derive(Debug)]
pub enum Expr {
    Identifier(Rc<str>),
    Number(f64),
    Bool(bool),
    Assign(NodeId, NodeId),
    // Unary operators
    Neg(NodeId),
    Binary {
        lhs: NodeId,
        op: BinaryOp,
        rhs: NodeId,
    },
}

struct ExprView<'a> {
    expr_id: NodeId,
    node_map: &'a SlotMap<NodeId, Expr>,
}

impl<'a> ExprView<'a> {
    fn with_id(&self, id: &NodeId) -> Self {
        Self {
            expr_id: *id,
            node_map: self.node_map,
        }
    }

    fn eval(&self, cx: &mut Context) -> Result<Value, RuntimeError> {
        match &self.node_map[self.expr_id] {
            Expr::Identifier(name) => cx
                .get_symbol(name)
                .ok_or_else(|| RuntimeError::new(format!("Undefined symbol {}", name))),
            Expr::Number(value) => Ok(Value::Number(*value)),
            Expr::Bool(value) => Ok(Value::Bool(*value)),
            Expr::Assign(node_id, node_id1) => {
                let identifier = match &self.node_map[*node_id] {
                    Expr::Identifier(id) => id,
                    _ => return Err(RuntimeError::new(format!("Expected identifier"))),
                };
                let value = self.with_id(node_id1).eval(cx)?;
                cx.set_symbol(identifier.clone(), value);
                Ok(Value::Unit)
            }
            Expr::Neg(node_id) => todo!(),
            Expr::Binary { op, lhs, rhs } => {
                let lhs = self.with_id(lhs).eval(cx)?;
                let rhs = self.with_id(rhs).eval(cx)?;
                match op {
                    BinaryOp::Add => lhs.try_add(&rhs),
                    BinaryOp::Sub => lhs.try_sub(&rhs),
                    BinaryOp::Mul => lhs.try_mul(&rhs),
                    BinaryOp::Div => lhs.try_div(&rhs),
                    BinaryOp::Eq => Ok(Value::Bool(lhs.eq(&rhs))),
                    BinaryOp::NotEq => Ok(Value::Bool(!lhs.eq(&rhs))),
                    BinaryOp::Less => lhs.lt(&rhs).map(Value::Bool),
                    BinaryOp::LessEq => lhs.leq(&rhs).map(Value::Bool),
                    BinaryOp::Greater => lhs.gt(&rhs).map(Value::Bool),
                    BinaryOp::GreaterEq => lhs.geq(&rhs).map(Value::Bool),
                    _ => panic!(),
                }
            }
        }
    }
}

impl<'a> Debug for ExprView<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.node_map[self.expr_id] {
            Expr::Identifier(arg0) => f.debug_tuple("Identifier").field(arg0).finish(),
            Expr::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Expr::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Expr::Neg(arg0) => f.debug_tuple("Neg").field(arg0).finish(),
            Expr::Assign(node_id, node_id1) => f
                .debug_tuple("Assign")
                .field(&self.with_id(node_id))
                .field(&self.with_id(node_id1))
                .finish(),
            Expr::Binary { op, lhs, rhs } => f
                .debug_tuple(op.as_str())
                .field(&self.with_id(lhs))
                .field(&self.with_id(rhs))
                .finish(),
        }
    }
}

type Stream<'i> = Stateful<Tokens<'i>, &'i mut SlotMap<NodeId, Expr>>;

fn parse_expr<'i>() -> impl Parser<Stream<'i>, NodeId, ErrMode<ContextError>> {
    move |i: &mut Stream<'i>| expression(term).infix(infix).parse_next(i)
}

fn infix<'i>(i: &mut Stream<'i>) -> ModalResult<Infix<Stream<'i>, NodeId, ErrMode<ContextError>>> {
    any.verify_map(|token: &Token<'i>| match token.kind {
        TokenKind::Op(op) => match op {
            Oper::Mul => Some(Infix::Left(16, |i: &mut Stream, lhs, rhs| {
                Ok(i.state.insert(Expr::Binary {
                    op: BinaryOp::Mul,
                    lhs,
                    rhs,
                }))
            })),
            Oper::Div => Some(Infix::Left(16, |i: &mut Stream, lhs, rhs| {
                Ok(i.state.insert(Expr::Binary {
                    op: BinaryOp::Div,
                    lhs,
                    rhs,
                }))
            })),
            Oper::Add => Some(Infix::Left(14, |i: &mut Stream, lhs, rhs| {
                Ok(i.state.insert(Expr::Binary {
                    op: BinaryOp::Add,
                    lhs,
                    rhs,
                }))
            })),
            Oper::Sub => Some(Infix::Left(14, |i: &mut Stream, lhs, rhs| {
                Ok(i.state.insert(Expr::Binary {
                    op: BinaryOp::Sub,
                    lhs,
                    rhs,
                }))
            })),
            Oper::LessEq => Some(Infix::Neither(12, |i: &mut Stream, lhs, rhs| {
                Ok(i.state.insert(Expr::Binary {
                    op: BinaryOp::LessEq,
                    lhs,
                    rhs,
                }))
            })),
            Oper::Less => Some(Infix::Neither(12, |i: &mut Stream, lhs, rhs| {
                Ok(i.state.insert(Expr::Binary {
                    op: BinaryOp::Less,
                    lhs,
                    rhs,
                }))
            })),
            Oper::GreaterEq => Some(Infix::Neither(12, |i: &mut Stream, lhs, rhs| {
                Ok(i.state.insert(Expr::Binary {
                    op: BinaryOp::GreaterEq,
                    lhs,
                    rhs,
                }))
            })),
            Oper::Greater => Some(Infix::Neither(12, |i: &mut Stream, lhs, rhs| {
                Ok(i.state.insert(Expr::Binary {
                    op: BinaryOp::Greater,
                    lhs,
                    rhs,
                }))
            })),
            Oper::Eq => Some(Infix::Neither(12, |i: &mut Stream, lhs, rhs| {
                Ok(i.state.insert(Expr::Binary {
                    op: BinaryOp::Eq,
                    lhs,
                    rhs,
                }))
            })),
            Oper::Assign => Some(Infix::Neither(12, |i: &mut Stream, a, b| {
                Ok(i.state.insert(Expr::Assign(a, b)))
            })),
            _ => None,
        },
        _ => None,
    })
    .parse_next(i)
}

fn term<'i>(i: &mut Stream<'i>) -> ModalResult<NodeId> {
    let val = any
        .verify_map(|token: &Token<'i>| match token.kind {
            TokenKind::Identifier(name) => Some(Expr::Identifier(Rc::from(name))),
            TokenKind::Number(number) => Some(Expr::Number(number)),
            TokenKind::Keyword(keyword) if keyword == Keyword::True => Some(Expr::Bool(true)),
            TokenKind::Keyword(keyword) if keyword == Keyword::False => Some(Expr::Bool(true)),
            _ => None,
        })
        .parse_next(i)?;
    Ok(i.state.insert(val))
}

struct Expressions {
    nodes: SlotMap<NodeId, Expr>,
    root_id: NodeId,
}

impl Expressions {
    pub fn parse(i: &str) -> Self {
        let mut nodes = SlotMap::default();

        let tokens = tokens(i).unwrap();
        let stream = Stateful {
            input: TokenSlice::new(&tokens),
            state: &mut nodes,
        };
        let root_id = parse_expr().parse(stream).unwrap();
        Self { nodes, root_id }
    }

    pub fn root(&self) -> ExprView<'_> {
        ExprView {
            expr_id: self.root_id,
            node_map: &self.nodes,
        }
    }
}

impl Debug for Expressions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.root().fmt(f)
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

fn main() {
    let expr = Expressions::parse("10 + 20 / 2 * 4 > 5");
    let mut cx = Context::new();
    let value = expr.root().eval(&mut cx);
    println!("{:?} -> {:?}", expr, value);
}
