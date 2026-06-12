use std::{
    cell::Cell,
    collections::{HashSet, VecDeque},
    fmt::{Debug, Display},
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
            TokenKind::Literal(value) => match value {
                Literal::Bool(value) => self.push_expr(ExprKind::Bool(value), token.span),
                Literal::Number(value) => self.push_expr(ExprKind::Number(value), token.span),
            },
            TokenKind::Identifier(name) => {
                let atom = self.atoms.get_or_intern(name);
                self.push_expr(ExprKind::Identifier(atom), token.span)
            }
            TokenKind::Keyword(keyword) => match keyword {
                Keyword::If => todo!(),
                Keyword::Fn => {
                    let mut span = token.span;
                    i.expect_token(TokenKind::Op(Oper::LParen))?;

                    let mut args = Vec::new();
                    while let Some(t) = i.peek() {
                        let TokenKind::Identifier(id) = t.kind else {
                            break;
                        };
                        i.next()?;
                        args.push(self.atoms.get_or_intern(id));

                        if !i.match_token(TokenKind::Op(Oper::Comma)).is_some() {
                            break;
                        }
                    }
                    i.expect_token(TokenKind::Op(Oper::RParen))?;

                    let body = self.parse_expr(i, 0)?;
                    let mut captures = HashSet::new();
                    self.find_captures(body, &mut |atom| {
                        if !args.contains(&atom) {
                            captures.insert(atom);
                        }
                    });
                    let fn_def = FunctionDef {
                        args,
                        body,
                        captures,
                    };
                    self.push_expr(ExprKind::FunctionDef(fn_def), span)
                }
                _ => {
                    return Err(ParseError::unexpected_token(token, "expression"));
                }
            },
            TokenKind::Op(Oper::LParen) => {
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
        };

        while let Some(token) = i.peek() {
            let op = match token.kind {
                TokenKind::Op(op) => op,
                _ => return Err(ParseError::unexpected_token(token, "operator")),
            };

            if let Some((op, binding_power)) = postfix_op(op) {
                if binding_power < min_binding_power {
                    break;
                }
                i.next()?;
                match op {
                    PostfixOp::FunctionCall => {
                        let mut args = Vec::new();
                        if i.peek()
                            .is_some_and(|t| t.kind != TokenKind::Op(Oper::RParen))
                        {
                            loop {
                                args.push(self.parse_expr(i, 0)?);
                                if !i.match_token(TokenKind::Op(Oper::Comma)).is_some() {
                                    break;
                                }
                            }
                        }

                        i.expect_token(TokenKind::Op(Oper::RParen))?;
                        lhs = self.push_expr(
                            ExprKind::FunctionCall { func: lhs, args },
                            self.expr_span(lhs, &[]),
                        );
                        continue;
                    }
                }
            }

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

    fn find_captures(&self, id: ExprId, f: &mut impl FnMut(Atom)) {
        let mut rem = self.id_buffer.take();
        rem.clear();
        rem.push_back(id);
        while let Some(id) = rem.pop_front() {
            match &self.exprs[id.0].kind {
                ExprKind::Identifier(atom) => f(*atom),
                ExprKind::Number(_) | ExprKind::Bool(_) => {}
                ExprKind::Paren(expr_id) => rem.push_back(*expr_id),
                ExprKind::FunctionCall { args, .. } => rem.extend(args.iter()),
                ExprKind::FunctionDef(function_def) => {
                    for c in function_def.captures.iter() {
                        f(*c)
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

    fn match_token(&mut self, kind: TokenKind) -> Option<&'i Token<'i>> {
        if let Some((first, rem)) = self.tokens.split_first() {
            if first.kind == kind {
                self.tokens = rem;
                Some(first)
            } else {
                None
            }
        } else {
            None
        }
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
    kind: ExprKind,
    span: SourceSpan,
}

#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Identifier(Atom),
    Number(f64),
    Bool(bool),
    Paren(ExprId),
    FunctionCall {
        func: ExprId,
        args: Vec<ExprId>,
    },
    FunctionDef(FunctionDef),
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

#[derive(Debug, PartialEq)]
pub struct FunctionDef {
    pub args: Vec<Atom>,
    pub captures: HashSet<Atom>,
    pub body: ExprId,
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
            ExprKind::Paren(expr_id) => f
                .debug_tuple("Paren")
                .field(&self.with_id(*expr_id))
                .finish(),
            ExprKind::Block { .. } => f.debug_tuple("Block").finish(),
            ExprKind::FunctionCall { func, args } => f
                .debug_struct("FunctionCall")
                .field("func", &self.with_id(*func))
                .finish(),
            ExprKind::FunctionDef(_) => todo!(),
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

    fn fmt_ast(e: ExprView, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match e.expr() {
            ExprKind::Identifier(atom) => {
                write!(f, "ID {}", e.exprs.atoms.resolve(atom))
            }
            ExprKind::Number(n) => write!(f, "{}", n),
            ExprKind::Bool(v) => write!(f, "{}", v),
            ExprKind::Paren(expr_id) => {
                f.write_char('(')?;
                fmt_ast(e.with_id(*expr_id), f)?;
                f.write_char(')')
            }
            ExprKind::FunctionCall { func, args } => todo!(),
            ExprKind::FunctionDef(function_def) => todo!(),
            ExprKind::Unary { op, operand } => {
                write!(f, "({} ", op.as_str())?;
                fmt_ast(e.with_id(*operand), f)?;
                f.write_char(')')
            }
            ExprKind::Binary { lhs, op, rhs } => {
                write!(f, "({} ", op.as_str())?;
                fmt_ast(e.with_id(*lhs), f)?;
                f.write_char(' ')?;
                fmt_ast(e.with_id(*rhs), f)?;
                f.write_char(')')
            }
            ExprKind::Block { .. } => todo!(),
        }
    }

    struct TestPrinter<'a>(ExprView<'a>);
    impl<'a> Display for TestPrinter<'a> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            fmt_ast(self.0, f)
        }
    }

    fn parse_and_fmt(i: &str) -> String {
        let tokens = tokens(i).unwrap();
        let mut expressions = Expressions::new();
        let e = expressions.parse(&tokens).unwrap();
        TestPrinter(e).to_string()
    }

    #[test]
    fn binary_op_parsing() {
        assert_eq!(parse_and_fmt("1 + 1"), "(+ 1 1)");
        assert_eq!(parse_and_fmt("1 + 1 < 5"), "(< (+ 1 1) 5)");
        assert_eq!(parse_and_fmt("1 * 2 + 1"), "(+ (* 1 2) 1)");
        assert_eq!(parse_and_fmt("1 + 1 * 2"), "(+ 1 (* 1 2))");
    }
}
