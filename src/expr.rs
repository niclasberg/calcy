use std::{
    cell::Cell,
    collections::{HashSet, VecDeque},
    fmt::{Debug, Display},
    ops::Index,
};

use crate::{
    atom::{Atom, Atoms},
    lexer::{Keyword, Literal, Oper, SourceSpan, Token, TokenKind},
};

pub struct Expressions {
    exprs: Vec<Expr>,
    atoms: Atoms,
    id_buffer: Cell<VecDeque<ExprId>>,
}

impl Expressions {
    pub fn new() -> Self {
        Self {
            exprs: Vec::new(),
            atoms: Atoms::new(),
            id_buffer: Cell::new(VecDeque::new()),
        }
    }

    pub fn parse(&mut self, tokens: &[Token]) -> Result<Expr, ParseError> {
        let eof_token = Token {
            kind: TokenKind::Eof,
            span: tokens
                .last()
                .map(|t| t.span.end..t.span.end)
                .unwrap_or(0..0)
                .into(),
        };
        let mut token_stream = TokenStream {
            tokens,
            eof_token: &eof_token,
        };
        let expr = self.parse_block(&mut token_stream)?;
        token_stream.expect_token(TokenKind::Eof)?;
        Ok(expr)
    }

    /// Parse multiple ;-separated expressions
    fn parse_block(&mut self, i: &mut TokenStream) -> Result<Expr, ParseError> {
        let first = self.parse_statement_or_expr(i)?;
        if i.peek().kind != TokenKind::Op(Oper::SemiColon) {
            return Ok(first);
        }

        let mut span = first.span;
        let mut children = vec![self.push_expr(first)];
        while i.match_token(TokenKind::Op(Oper::SemiColon)).is_some() {
            let expr = self.parse_statement_or_expr(i)?;
            span = span.join(&expr.span);
            children.push(self.push_expr(expr));
        }

        Ok(Expr::new(ExprKind::Block { children }, span))
    }

    fn parse_statement_or_expr(&mut self, i: &mut TokenStream) -> Result<Expr, ParseError> {
        match i.peek().kind {
            TokenKind::Keyword(Keyword::Let) => self.parse_let(i),
            _ => self.parse_expr(i, 0),
        }
    }

    fn parse_let(&mut self, i: &mut TokenStream) -> Result<Expr, ParseError> {
        let let_token = i.expect_token(TokenKind::Keyword(Keyword::Let))?;
        let id = self
            .try_parse_identifier(i)
            .ok_or(ParseError::unexpected_token(i.peek(), "identifier"))?;
        i.expect_token(TokenKind::Op(Oper::Assign))?;
        let value_expr = self.parse_expr(i, 0)?;
        let span = let_token.span.join(&value_expr.span);
        let value = self.push_expr(value_expr);
        Ok(Expr::new(ExprKind::Let { id, value }, span))
    }

    fn try_parse_identifier(&mut self, i: &mut TokenStream) -> Option<Atom> {
        i.match_map(|t| match t.kind {
            TokenKind::Identifier(id) => Some(self.atoms.get_or_intern(id)),
            _ => None,
        })
    }

    fn parse_expr(
        &mut self,
        i: &mut TokenStream,
        min_binding_power: u8,
    ) -> Result<Expr, ParseError> {
        let token = i.next();
        let mut lhs = match token.kind {
            TokenKind::Literal(value) => match value {
                Literal::Bool(value) => Expr::new(ExprKind::Bool(value), token.span),
                Literal::Number(value) => Expr::new(ExprKind::Number(value), token.span),
            },
            TokenKind::Identifier(name) => {
                let atom = self.atoms.get_or_intern(name);
                Expr::new(ExprKind::Identifier(atom), token.span)
            }
            TokenKind::Keyword(keyword) => match keyword {
                Keyword::If => todo!(),
                Keyword::Fn => {
                    let span = token.span;
                    i.expect_token(TokenKind::Op(Oper::LParen))?;

                    let mut args = Vec::new();
                    while let Some(atom) = self.try_parse_identifier(i) {
                        args.push(atom);
                        if i.match_token(TokenKind::Op(Oper::Comma)).is_none() {
                            break;
                        }
                    }
                    i.expect_token(TokenKind::Op(Oper::RParen))?;

                    let body_expr = self.parse_expr(i, 0)?;
                    let span = span.join(&body_expr.span);
                    let body = self.push_expr(body_expr);
                    let mut captures = HashSet::new();
                    self.find_captures(body, &mut |atom| {
                        if !args.contains(&atom) {
                            captures.insert(atom);
                        }
                    });
                    Expr::new(
                        ExprKind::FunctionDef {
                            args,
                            captures,
                            body,
                        },
                        span,
                    )
                }
                _ => {
                    return Err(ParseError::unexpected_token(token, "expression"));
                }
            },
            TokenKind::Op(Oper::LParen) => {
                let expr = self.parse_block(i)?;
                i.expect_token(TokenKind::Op(Oper::RParen))?;
                expr
            }
            TokenKind::Op(op) => {
                let Some((op, bp)) = prefix_op(op) else {
                    return Err(ParseError::unexpected_token(token, "expression"));
                };
                let inner = self.parse_expr(i, bp)?;
                let span = token.span.join(&inner.span);
                Expr::new(
                    ExprKind::Unary {
                        op,
                        operand: self.push_expr(inner),
                    },
                    span,
                )
            }
            TokenKind::Eof => {
                return Err(ParseError::unexpected_token(token, "expression"));
            }
        };

        while !i.is_eof() {
            let token = i.peek();
            let op = match token.kind {
                TokenKind::Op(op) => op,
                _ => return Err(ParseError::unexpected_token(token, "operator")),
            };

            if let Some((op, binding_power)) = postfix_op(op) {
                if binding_power < min_binding_power {
                    break;
                }
                i.next();
                match op {
                    PostfixOp::FunctionCall => {
                        let mut args = Vec::new();
                        if i.peek().kind != TokenKind::Op(Oper::RParen) {
                            loop {
                                let arg_expr = self.parse_expr(i, 0)?;
                                args.push(self.push_expr(arg_expr));
                                if i.match_token(TokenKind::Op(Oper::Comma)).is_none() {
                                    break;
                                }
                            }
                        }

                        let r_paren_token = i.expect_token(TokenKind::Op(Oper::RParen))?;
                        let span = lhs.span.join(&r_paren_token.span);
                        lhs = Expr::new(
                            ExprKind::FunctionCall {
                                func: self.push_expr(lhs),
                                args,
                            },
                            span,
                        );
                        continue;
                    }
                }
            }

            if let Some((op, left_binding_power, right_binding_power)) = infix_op(op) {
                if left_binding_power < min_binding_power {
                    break;
                }
                i.next();
                let rhs = self.parse_expr(i, right_binding_power)?;
                let span = lhs.span.join(&rhs.span);
                lhs = Expr::new(
                    ExprKind::Binary {
                        lhs: self.push_expr(lhs),
                        op,
                        rhs: self.push_expr(rhs),
                    },
                    span,
                );
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn find_captures(&self, id: ExprId, f: &mut impl FnMut(Atom)) {
        let mut rem = self.id_buffer.take();
        rem.clear();
        rem.push_back(id);
        let mut locals = HashSet::new();
        while let Some(id) = rem.pop_front() {
            let e = &self.exprs[id.0];
            match &e.kind {
                ExprKind::Identifier(atom) => {
                    if !locals.contains(atom) {
                        f(*atom)
                    }
                }
                ExprKind::Let { id, value } => {
                    locals.insert(id);
                    rem.push_back(*value);
                }
                ExprKind::Number(_) | ExprKind::Bool(_) => {}
                ExprKind::FunctionCall { args, .. } => rem.extend(args.iter()),
                ExprKind::FunctionDef { captures, .. } => {
                    for c in captures.iter() {
                        if !locals.contains(c) {
                            f(*c)
                        }
                    }
                }
                ExprKind::Unary { operand, .. } => rem.push_back(*operand),
                ExprKind::Binary { lhs, rhs, .. } => {
                    rem.push_back(*lhs);
                    rem.push_back(*rhs);
                }
                ExprKind::Block { children } => rem.extend(children.iter()),
            }
        }
        self.id_buffer.set(rem);
    }

    fn push_expr(&mut self, expr: Expr) -> ExprId {
        let id = self.exprs.len();
        self.exprs.push(expr);
        ExprId(id)
    }

    pub fn get_expr(&self, id: ExprId) -> Option<&Expr> {
        self.exprs.get(id.0)
    }
}

impl Index<ExprId> for Expressions {
    type Output = Expr;

    fn index(&self, index: ExprId) -> &Self::Output {
        &self.exprs[index.0]
    }
}

struct TokenStream<'i> {
    tokens: &'i [Token<'i>],
    eof_token: &'i Token<'i>,
}

impl<'i> TokenStream<'i> {
    fn next(&mut self) -> &'i Token<'i> {
        if let Some((first, rem)) = self.tokens.split_first() {
            self.tokens = rem;
            first
        } else {
            self.eof_token
        }
    }

    fn is_eof(&self) -> bool {
        self.tokens.is_empty()
    }

    fn peek(&self) -> &'i Token<'i> {
        self.tokens.first().unwrap_or(self.eof_token)
    }

    fn match_map<R>(&mut self, f: impl FnOnce(&Token) -> Option<R>) -> Option<R> {
        if let Some((first, rem)) = self.tokens.split_first() {
            let r = f(first);
            if r.is_some() {
                self.tokens = rem;
            }
            r
        } else {
            None
        }
    }

    fn match_with(&mut self, f: impl FnOnce(&Token) -> bool) -> Option<&'i Token<'i>> {
        if let Some((first, rem)) = self.tokens.split_first() {
            if f(first) {
                self.tokens = rem;
                Some(first)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn match_token(&mut self, kind: TokenKind) -> Option<&'i Token<'i>> {
        self.match_with(|t| t.kind == kind)
    }

    fn expect(
        &mut self,
        f: impl Fn(&Token) -> bool,
        expected: impl ToString,
    ) -> Result<&'i Token<'i>, ParseError> {
        let token = self.next();
        if f(token) {
            Ok(token)
        } else {
            Err(ParseError::unexpected_token(token, expected))
        }
    }

    fn expect_map<R>(
        &mut self,
        f: impl Fn(&Token) -> Option<R>,
        expected: impl ToString,
    ) -> Result<R, ParseError> {
        let token = self.next();
        f(token).ok_or_else(|| ParseError::unexpected_token(token, expected))
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

const fn postfix_op(op: Oper) -> Option<(PostfixOp, u8)> {
    match op {
        Oper::LParen => Some((PostfixOp::FunctionCall, 30)),
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
    pub kind: ExprKind,
    pub span: SourceSpan,
}

impl Expr {
    pub fn new(kind: ExprKind, span: SourceSpan) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Identifier(Atom),
    Number(f64),
    Bool(bool),
    FunctionCall {
        func: ExprId,
        args: Vec<ExprId>,
    },
    Let {
        id: Atom,
        value: ExprId,
    },
    FunctionDef {
        args: Vec<Atom>,
        captures: HashSet<Atom>,
        body: ExprId,
    },
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
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Eq => "==",
            BinaryOp::NotEq => "!=",
            BinaryOp::Less => "<",
            BinaryOp::LessEq => "<=",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEq => ">=",
            BinaryOp::Assign => "=",
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

#[derive(Debug, Clone, Copy, PartialEq)]
enum PostfixOp {
    FunctionCall,
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
            ExprKind::Identifier(atom) => f
                .debug_tuple("Identifier")
                .field(&self.exprs.atoms.resolve(atom))
                .finish(),
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
            ExprKind::Block { .. } => f.debug_tuple("Block").finish(),
            ExprKind::FunctionCall { func, args } => f
                .debug_struct("FunctionCall")
                .field("func", &self.with_id(*func))
                .finish(),
            ExprKind::FunctionDef { .. } => todo!(),
            ExprKind::Let { id, value } => todo!(),
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

#[cfg(test)]
mod tests {
    use std::fmt::{Formatter, Write};

    use crate::lexer::tokens;

    use super::*;

    fn fmt_ast(
        expr: &Expr,
        expressions: &Expressions,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match &expr.kind {
            ExprKind::Identifier(atom) => {
                write!(f, "ID {}", expressions.atoms.resolve(atom))
            }
            ExprKind::Number(n) => write!(f, "{}", n),
            ExprKind::Bool(v) => write!(f, "{}", v),
            ExprKind::FunctionCall { func, args } => todo!(),
            ExprKind::FunctionDef { .. } => todo!(),
            ExprKind::Unary { op, operand } => {
                write!(f, "({} ", op.as_str())?;
                fmt_ast(&expressions[*operand], expressions, f)?;
                f.write_char(')')
            }
            ExprKind::Binary { lhs, op, rhs } => {
                write!(f, "({} ", op.as_str())?;
                fmt_ast(&expressions[*lhs], expressions, f)?;
                f.write_char(' ')?;
                fmt_ast(&expressions[*rhs], expressions, f)?;
                f.write_char(')')
            }
            ExprKind::Block { .. } => todo!(),
            ExprKind::Let { .. } => todo!(),
        }
    }

    struct TestPrinter<'a>(&'a Expr, &'a Expressions);
    impl<'a> Display for TestPrinter<'a> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            fmt_ast(self.0, self.1, f)
        }
    }

    fn parse_and_fmt(i: &str) -> String {
        let tokens = tokens(i).unwrap();
        let mut expressions = Expressions::new();
        let e = expressions.parse(&tokens).unwrap();
        TestPrinter(&e, &expressions).to_string()
    }

    #[test]
    fn binary_op_parsing() {
        assert_eq!(parse_and_fmt("1 + 1"), "(+ 1 1)");
        assert_eq!(parse_and_fmt("1 -1"), "(- 1 1)");
        assert_eq!(parse_and_fmt("-1--1"), "(- -1 -1)");
        assert_eq!(parse_and_fmt("1 - 1"), "(- 1 1)");
        assert_eq!(parse_and_fmt("1 + 1 < 5"), "(< (+ 1 1) 5)");
        assert_eq!(parse_and_fmt("1 * 2 + 1"), "(+ (* 1 2) 1)");
        assert_eq!(parse_and_fmt("1 + 1 * 2"), "(+ 1 (* 1 2))");
    }
}
