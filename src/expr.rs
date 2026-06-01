use std::{
    fmt::{Debug, Display},
    ops::Range,
    rc::Rc,
};

use winnow::stream::Stream;

use crate::lexer::{Keyword, Oper, SourceSpan, Token, TokenKind, Tokens};

pub struct Expressions {
    exprs: Vec<Expr>,
    expr_lists: Vec<ExprId>,
}

impl Expressions {
    pub fn new() -> Self {
        Self {
            exprs: Vec::new(),
            expr_lists: Vec::new(),
        }
    }

    pub fn parse(tokens: &[Token]) -> Result<Self, ParseError> {
        let mut tokens = Tokens::new(tokens);
        let mut this = Self::new();
        this.parse_exprs(&mut tokens)?;
        Ok(this)
    }

    /// Parse multiple ;-separated expressions
    fn parse_exprs(&mut self, i: &mut Tokens) -> Result<ExprId, ParseError> {
        self.parse_expr(i, 0)
    }

    fn parse_expr<'i>(
        &mut self,
        i: &mut Tokens<'i>,
        min_binding_power: u8,
    ) -> Result<ExprId, ParseError> {
        let token = i.next_token().ok_or(ParseError::eof(i))?;
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
            TokenKind::LParen => {
                let inner = self.parse_expr(i, 0)?;
                let closing_paren = i.next_token().ok_or(ParseError::eof(i))?;
                if matches!(closing_paren.kind, TokenKind::RParen) {
                    return Err(ParseError::unexpected_token(
                        closing_paren,
                        TokenKind::RParen,
                    ));
                }
                self.push_expr(ExprKind::Paren(inner), token.span.join(&closing_paren.span))
            }
            TokenKind::RParen => todo!(),
            TokenKind::Op(op) => {
                if let Some((_, bp)) = prefix_binding_power(op) {}
                todo!()
            }
        };

        while let Some(token) = i.peek_token() {
            let op = match token.kind {
                TokenKind::Op(op) => op,
                _ => return Err(ParseError::unexpected_token(token, "operator")),
            };

            if let Some((op, left_binding_power, right_binding_power)) = infix_op(op) {
                if left_binding_power < min_binding_power {
                    break;
                }
                i.next_token();
                let rhs = self.parse_expr(i, right_binding_power)?;
                let span = self
                    .get_expr(lhs)
                    .unwrap()
                    .span
                    .join(&self.get_expr(rhs).unwrap().span);
                lhs = self.push_expr(ExprKind::Binary { lhs, op, rhs }, span);
                continue;
            }

            break;
        }

        Ok(lhs)
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

const fn prefix_binding_power(op: Oper) -> Option<((), u8)> {
    match op {
        Oper::Sub => todo!(),
        Oper::Not => todo!(),
        _ => None,
    }
}

const fn infix_op(op: Oper) -> Option<(BinaryOp, u8, u8)> {
    const ASSIGN_BP: u8 = 1;
    const EQ_BP: u8 = 10;
    const CMP_BP: u8 = 12;
    const MUL_DIV_BP: u8 = 20;
    const ADD_SUB_BP: u8 = 22;
    match op {
        Oper::Assign => Some((BinaryOp::Assign, ASSIGN_BP, ASSIGN_BP)),
        Oper::Eq => Some((BinaryOp::Eq, EQ_BP, EQ_BP + 1)),
        Oper::NotEq => Some((BinaryOp::NotEq, EQ_BP, EQ_BP + 1)),
        Oper::Less => Some((BinaryOp::Less, CMP_BP, CMP_BP + 1)),
        Oper::LessEq => Some((BinaryOp::LessEq, CMP_BP, CMP_BP + 1)),
        Oper::Greater => Some((BinaryOp::Greater, CMP_BP, CMP_BP + 1)),
        Oper::GreaterEq => Some((BinaryOp::GreaterEq, CMP_BP, CMP_BP + 1)),
        Oper::Mul => Some((BinaryOp::Mul, MUL_DIV_BP, MUL_DIV_BP + 1)),
        Oper::Div => Some((BinaryOp::Div, MUL_DIV_BP, MUL_DIV_BP + 1)),
        Oper::Add => Some((BinaryOp::Add, ADD_SUB_BP, ADD_SUB_BP + 1)),
        Oper::Sub => Some((BinaryOp::Sub, ADD_SUB_BP, ADD_SUB_BP + 1)),
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
        /// Offset into the expr_lists vec
        expr_start: usize,
        length: usize,
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

impl UnaryOp {}

pub struct ParseError {
    kind: ParseErrorKind,
    span: SourceSpan,
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

    fn eof(tokens: &Tokens) -> Self {
        let end = tokens.previous_tokens().next().unwrap().span.end;
        Self {
            kind: ParseErrorKind::UnexpectedEOF,
            span: (end..end + 1).into(),
        }
    }
}

pub enum ParseErrorKind {
    UnexpectedToken { found: String, expected: String },
    UnexpectedEOF,
}
