use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::lexer::{Keyword, Oper, SourceSpan, Token, TokenKind};

pub struct Expressions {
    exprs: Vec<Expr>,
}

impl Expressions {
    pub fn new() -> Self {
        Self { exprs: Vec::new() }
    }

    pub fn parse<'s>(&'s mut self, tokens: &[Token]) -> Result<ExprView<'s>, ParseError> {
        let mut token_stream = TokenStream {
            tokens,
            eof_span: tokens
                .last()
                .map(|t| t.span.end..t.span.end)
                .unwrap_or(0..0)
                .into(),
        };
        let id = self.parse_exprs(&mut token_stream)?;
        Ok(ExprView { id, exprs: self })
    }

    /// Parse multiple ;-separated expressions
    fn parse_exprs(&mut self, i: &mut TokenStream) -> Result<ExprId, ParseError> {
        let first = self.parse_expr(i, 0)?;
        if i.is_eof() {
            return Ok(first);
        }

        let mut children = vec![first];

        while !i.is_eof() {
            i.expect_token(TokenKind::Op(Oper::SemiColon))?;
            children.push(self.parse_expr(i, 0)?);
        }
        let span = self.expr_span(first, children.as_slice());
        Ok(self.push_expr(ExprKind::Block { children }, span))
    }

    fn parse_expr(
        &mut self,
        i: &mut TokenStream,
        min_binding_power: u8,
    ) -> Result<ExprId, ParseError> {
        let token = i.next()?;
        let mut lhs = match token.kind {
            TokenKind::Number(value) => self.push_expr(ExprKind::Number(value), token.span),
            TokenKind::Identifier(name) => {
                self.push_expr(ExprKind::Identifier(Rc::from(name)), token.span)
            }
            TokenKind::Keyword(keyword) => match keyword {
                Keyword::If => todo!(),
                Keyword::True => self.push_expr(ExprKind::Bool(true), token.span),
                Keyword::False => self.push_expr(ExprKind::Bool(false), token.span),
                _ => {
                    return Err(ParseError::unexpected_token(token, "expression"));
                }
            },
            TokenKind::Op(op) if op == Oper::LParen => {
                let inner = self.parse_expr(i, 0)?;
                let closing_paren = i.expect_token(TokenKind::Op(Oper::RParen))?;
                self.push_expr(ExprKind::Paren(inner), token.span.join(&closing_paren.span))
            }
            TokenKind::Op(op) => {
                let Some((op, bp)) = prefix_op(op) else {
                    return Err(ParseError::unexpected_token(token, "expression"));
                };
                let inner = self.parse_expr(i, bp)?;
                self.push_expr(ExprKind::Unary { op, operand: inner }, token.span)
            }
            _ => return Err(ParseError::unexpected_token(token, "expression")),
        };

        while let Some(token) = i.peek() {
            let op = match token.kind {
                TokenKind::Op(op) => op,
                _ => return Err(ParseError::unexpected_token(token, "operator")),
            };

            if let Some((op, left_binding_power, right_binding_power)) = infix_op(op) {
                if left_binding_power < min_binding_power {
                    break;
                }
                i.next()?;
                let rhs = self.parse_expr(i, right_binding_power)?;
                lhs = self.push_expr(
                    ExprKind::Binary { lhs, op, rhs },
                    self.expr_span(lhs, &[rhs]),
                );
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn expr_span(&self, head: ExprId, tail: &[ExprId]) -> SourceSpan {
        let mut span = self.get_expr(head).unwrap().span;
        for expr in tail {
            span = span.join(&self.get_expr(*expr).unwrap().span);
        }
        span
    }

    fn push_expr(&mut self, kind: ExprKind, span: SourceSpan) -> ExprId {
        let id = self.exprs.len();
        self.exprs.push(Expr { kind, span });
        ExprId(id)
    }

    pub fn get_expr(&self, id: ExprId) -> Option<&Expr> {
        self.exprs.get(id.0)
    }
}

struct TokenStream<'i> {
    tokens: &'i [Token<'i>],
    eof_span: SourceSpan,
}

impl<'i> TokenStream<'i> {
    fn next(&mut self) -> Result<&'i Token<'i>, ParseError> {
        if let Some((first, rem)) = self.tokens.split_first() {
            self.tokens = rem;
            Ok(first)
        } else {
            Err(ParseError::new(
                ParseErrorKind::UnexpectedEOF,
                self.eof_span,
            ))
        }
    }

    fn is_eof(&self) -> bool {
        self.tokens.is_empty()
    }

    fn peek(&self) -> Option<&'i Token<'i>> {
        self.tokens.first()
    }

    fn expect(
        &mut self,
        f: impl Fn(&Token) -> bool,
        expected: impl ToString,
    ) -> Result<&'i Token<'i>, ParseError> {
        let token = self.next()?;
        if f(token) {
            Ok(token)
        } else {
            Err(ParseError::unexpected_token(token, expected))
        }
    }

    fn expect_token(&mut self, kind: TokenKind) -> Result<&'i Token<'i>, ParseError> {
        self.expect(|t| t.kind == kind, kind)
    }
}

const fn prefix_op(op: Oper) -> Option<(UnaryOp, u8)> {
    match op {
        Oper::Sub => Some((UnaryOp::Neg, 32)),
        Oper::Not => Some((UnaryOp::Not, 30)),
        _ => None,
    }
}

const fn infix_op(op: Oper) -> Option<(BinaryOp, u8, u8)> {
    const ASSIGN_BP: u8 = 1;
    const EQ_BP: u8 = 10;
    const CMP_BP: u8 = 12;
    const ADD_SUB_BP: u8 = 20;
    const MUL_DIV_BP: u8 = 22;
    match op {
        Oper::Assign => Some((BinaryOp::Assign, ASSIGN_BP, ASSIGN_BP)),
        Oper::Eq => Some((BinaryOp::Eq, EQ_BP, EQ_BP + 1)),
        Oper::NotEq => Some((BinaryOp::NotEq, EQ_BP, EQ_BP + 1)),
        Oper::Less => Some((BinaryOp::Less, CMP_BP, CMP_BP + 1)),
        Oper::LessEq => Some((BinaryOp::LessEq, CMP_BP, CMP_BP + 1)),
        Oper::Greater => Some((BinaryOp::Greater, CMP_BP, CMP_BP + 1)),
        Oper::GreaterEq => Some((BinaryOp::GreaterEq, CMP_BP, CMP_BP + 1)),
        Oper::Add => Some((BinaryOp::Add, ADD_SUB_BP, ADD_SUB_BP + 1)),
        Oper::Sub => Some((BinaryOp::Sub, ADD_SUB_BP, ADD_SUB_BP + 1)),
        Oper::Mul => Some((BinaryOp::Mul, MUL_DIV_BP, MUL_DIV_BP + 1)),
        Oper::Div => Some((BinaryOp::Div, MUL_DIV_BP, MUL_DIV_BP + 1)),
        _ => None,
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprId(usize);

impl Debug for ExprId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

#[derive(Debug)]
pub struct Expr {
    kind: ExprKind,
    span: SourceSpan,
}

#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Identifier(Rc<str>),
    Number(f64),
    Bool(bool),
    Paren(ExprId),
    Unary {
        op: UnaryOp,
        operand: ExprId,
    },
    Binary {
        lhs: ExprId,
        op: BinaryOp,
        rhs: ExprId,
    },
    Block {
        children: Vec<ExprId>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

impl BinaryOp {
    pub const fn as_str(&self) -> &'static str {
        match self {
            BinaryOp::Add => "Add",
            BinaryOp::Sub => "Sub",
            BinaryOp::Mul => "Mul",
            BinaryOp::Div => "Div",
            BinaryOp::Eq => "Eq",
            BinaryOp::NotEq => "NotEq",
            BinaryOp::Less => "Less",
            BinaryOp::LessEq => "LessEq",
            BinaryOp::Greater => "Greater",
            BinaryOp::GreaterEq => "GreaterEq",
            BinaryOp::Assign => "Assign",
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl UnaryOp {
    pub const fn as_str(&self) -> &'static str {
        match self {
            UnaryOp::Neg => "Neg",
            UnaryOp::Not => "Not",
        }
    }
}

#[derive(Clone, Copy)]
pub struct ExprView<'a> {
    id: ExprId,
    exprs: &'a Expressions,
}

impl<'a> ExprView<'a> {
    pub fn with_id(&self, id: ExprId) -> Self {
        Self {
            id,
            exprs: self.exprs,
        }
    }

    pub fn expr(&self) -> &ExprKind {
        &self.exprs.get_expr(self.id).unwrap().kind
    }

    pub fn source_span(&self) -> SourceSpan {
        self.exprs.get_expr(self.id).unwrap().span
    }
}

impl<'a> Debug for ExprView<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.expr() {
            ExprKind::Identifier(arg0) => f.debug_tuple("Identifier").field(arg0).finish(),
            ExprKind::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            ExprKind::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            ExprKind::Unary { op, operand } => f
                .debug_tuple(op.as_str())
                .field(&self.with_id(*operand))
                .finish(),
            ExprKind::Binary { op, lhs, rhs } => f
                .debug_tuple(op.as_str())
                .field(&self.with_id(*lhs))
                .field(&self.with_id(*rhs))
                .finish(),
            ExprKind::Paren(expr_id) => f
                .debug_tuple("Paren")
                .field(&self.with_id(*expr_id))
                .finish(),
            ExprKind::Block { children } => f.debug_tuple("Block").finish(),
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: SourceSpan,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, span: SourceSpan) -> Self {
        Self { kind, span }
    }

    fn unexpected_token(found: &Token, expected: impl ToString) -> Self {
        Self {
            kind: ParseErrorKind::UnexpectedToken {
                found: found.kind.to_string(),
                expected: expected.to_string(),
            },
            span: found.span,
        }
    }
}

#[derive(Debug)]
pub enum ParseErrorKind {
    UnexpectedToken { found: String, expected: String },
    UnexpectedEOF,
}
