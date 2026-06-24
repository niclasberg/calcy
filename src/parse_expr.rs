use std::collections::HashSet;

use crate::{
    atom::Atom,
    expr::{
        ArrayElem, BinaryOp, Expr, ExprId, ExprKind, Expressions, FnArg, PostfixOp, TypeAnnotation,
        UnaryOp,
    },
    lexer::{Keyword, Literal, Oper, SourceSpan, Token, TokenKind},
};

pub fn parse(exprs: &mut Expressions, tokens: &[Token]) -> Result<ExprId, ParseError> {
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
    let expr = parse_block(exprs, &mut token_stream)?;
    token_stream.expect_token(TokenKind::Eof)?;
    Ok(exprs.push_expr(expr))
}

/// Parse multiple ;-separated expressions
fn parse_block(exprs: &mut Expressions, i: &mut TokenStream) -> Result<Expr, ParseError> {
    let first = parse_statement_or_expr(exprs, i)?;
    if i.peek().kind != TokenKind::Op(Oper::SemiColon) {
        return Ok(first);
    }

    let mut span = first.span;
    let mut children = vec![exprs.push_expr(first)];
    while i.match_token(TokenKind::Op(Oper::SemiColon)).is_some() {
        let expr = parse_statement_or_expr(exprs, i)?;
        span = span.join(&expr.span);
        children.push(exprs.push_expr(expr));
    }

    Ok(Expr::new(ExprKind::Block { children }, span))
}

fn parse_statement_or_expr(
    exprs: &mut Expressions,
    i: &mut TokenStream,
) -> Result<Expr, ParseError> {
    match i.peek().kind {
        TokenKind::Keyword(Keyword::Let) => parse_let(exprs, i),
        _ => parse_expr(exprs, i, 0),
    }
}

fn parse_let(exprs: &mut Expressions, i: &mut TokenStream) -> Result<Expr, ParseError> {
    let let_token = i.expect_token(TokenKind::Keyword(Keyword::Let))?;
    let id = parse_identifier(exprs, i)?;
    let type_annotation = parse_type_annotation(exprs, i)?;
    i.expect_token(TokenKind::Op(Oper::Assign))?;

    let value_expr = parse_expr(exprs, i, 0)?;
    let span = let_token.span.join(&value_expr.span);
    let value = exprs.push_expr(value_expr);
    Ok(Expr::new(
        ExprKind::Let {
            id,
            value,
            type_annotation,
        },
        span,
    ))
}

fn parse_identifier(exprs: &mut Expressions, i: &mut TokenStream) -> Result<Atom, ParseError> {
    i.match_map(|t| match t.kind {
        TokenKind::Identifier(id) => Some(exprs.get_or_intern(id)),
        _ => None,
    })
    .ok_or(ParseError::unexpected_token(i.peek(), "identifier"))
}

/// Parse an optional type annotation
fn parse_type_annotation(
    exprs: &mut Expressions,
    i: &mut TokenStream,
) -> Result<Option<TypeAnnotation>, ParseError> {
    if i.match_token(TokenKind::Op(Oper::Colon)).is_some() {
        let t = parse_type(exprs, i)?;
        Ok(Some(t))
    } else {
        Ok(None)
    }
}

fn parse_type(exprs: &mut Expressions, i: &mut TokenStream) -> Result<TypeAnnotation, ParseError> {
    fn inner(exprs: &mut Expressions, i: &mut TokenStream) -> Result<TypeAnnotation, ParseError> {
        let t = i.next();
        match t.kind {
            TokenKind::Keyword(Keyword::Bool) => Ok(TypeAnnotation::Bool),
            TokenKind::Keyword(Keyword::Float) => Ok(TypeAnnotation::Float),
            TokenKind::Op(Oper::LParen) => {
                let mut arg_types = Vec::new();
                if i.peek().kind != TokenKind::Op(Oper::RParen) {
                    loop {
                        arg_types.push(parse_type(exprs, i)?);
                        if i.match_token(TokenKind::Op(Oper::Comma)).is_none() {
                            break;
                        }
                    }
                }
                i.expect_token(TokenKind::Op(Oper::RParen))?;
                i.expect_token(TokenKind::Op(Oper::Arrow))?;

                let ret_type = parse_type(exprs, i)?;
                Ok(TypeAnnotation::Fn(arg_types, Box::new(ret_type)))
            }
            TokenKind::Op(Oper::LBracket) => {
                let elem_type = parse_type(exprs, i)?;
                i.expect_token(TokenKind::Op(Oper::RBracket))?;
                Ok(TypeAnnotation::Array(Box::new(elem_type)))
            }
            _ => Err(ParseError::unexpected_token(t, "type")),
        }
    }

    let first = inner(exprs, i)?;
    if i.match_token(TokenKind::Op(Oper::BinaryOr)).is_some() {
        let mut types = vec![first];
        loop {
            types.push(inner(exprs, i)?);
            if i.match_token(TokenKind::Op(Oper::BinaryOr)).is_none() {
                break;
            }
        }
        Ok(TypeAnnotation::Enum(types))
    } else {
        Ok(first)
    }
}

fn parse_expr(
    exprs: &mut Expressions,
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
            let atom = exprs.get_or_intern(name);
            Expr::new(ExprKind::Identifier(atom), token.span)
        }
        TokenKind::Keyword(keyword) => match keyword {
            Keyword::If => {
                let cond = parse_expr(exprs, i, 0)?;
                i.expect_token(TokenKind::Keyword(Keyword::Then))?;
                let lhs = parse_expr(exprs, i, 0)?;
                i.expect_token(TokenKind::Keyword(Keyword::Else))?;
                let rhs = parse_expr(exprs, i, 0)?;
                Expr::new(
                    ExprKind::IfThenElse {
                        cond: exprs.push_expr(cond),
                        lhs: exprs.push_expr(lhs),
                        rhs: exprs.push_expr(rhs),
                    },
                    token.span,
                )
            }
            Keyword::Fn => {
                let span = token.span;
                i.expect_token(TokenKind::Op(Oper::LParen))?;

                let mut args = Vec::new();
                if i.peek().kind != TokenKind::Op(Oper::RParen) {
                    loop {
                        let id = parse_identifier(exprs, i)?;
                        let type_annotation = parse_type_annotation(exprs, i)?;
                        args.push(FnArg {
                            id,
                            type_annotation,
                        });
                        if i.match_token(TokenKind::Op(Oper::Comma)).is_none() {
                            break;
                        }
                    }
                }

                i.expect_token(TokenKind::Op(Oper::RParen))?;
                let ret_type = parse_type_annotation(exprs, i)?;

                let body_expr = parse_expr(exprs, i, 0)?;
                let span = span.join(&body_expr.span);
                let body = exprs.push_expr(body_expr);
                let mut captures = HashSet::new();
                exprs.find_captures(body, &mut |atom| {
                    if !args.iter().any(|a| a.id == atom) {
                        captures.insert(atom);
                    }
                });
                Expr::new(
                    ExprKind::FunctionDef {
                        args,
                        captures,
                        body,
                        ret_type,
                    },
                    span,
                )
            }
            _ => {
                return Err(ParseError::unexpected_token(token, "expression"));
            }
        },
        TokenKind::Op(Oper::LParen) => {
            let expr = parse_block(exprs, i)?;
            i.expect_token(TokenKind::Op(Oper::RParen))?;
            expr
        }
        TokenKind::Op(Oper::LBracket) => {
            let mut values = Vec::new();
            if i.peek().kind != TokenKind::Op(Oper::RBracket) {
                loop {
                    let flatten = i.match_token(TokenKind::Op(Oper::Spread)).is_some();
                    let expr = parse_expr(exprs, i, 0)?;
                    let span = expr.span;
                    let elem = ArrayElem {
                        expr_id: exprs.push_expr(expr),
                        flatten,
                        span,
                    };
                    values.push(elem);
                    if i.match_token(TokenKind::Op(Oper::Comma)).is_none() {
                        break;
                    }
                }
            }
            let rbracket_token = i.expect_token(TokenKind::Op(Oper::RBracket))?;
            let span = token.span.join(&rbracket_token.span);
            Expr::new(ExprKind::Array(values), span)
        }
        TokenKind::Op(op) => {
            let Some((op, bp)) = prefix_op(op) else {
                return Err(ParseError::unexpected_token(token, "expression"));
            };
            let inner = parse_expr(exprs, i, bp)?;
            let span = token.span.join(&inner.span);
            Expr::new(
                ExprKind::Unary {
                    op,
                    operand: exprs.push_expr(inner),
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
            _ => break,
        };

        if let Some((op, binding_power)) = postfix_op(op) {
            if binding_power < min_binding_power {
                break;
            }
            i.next();

            lhs = match op {
                PostfixOp::FunctionCall => {
                    let mut args = Vec::new();
                    if i.peek().kind != TokenKind::Op(Oper::RParen) {
                        loop {
                            let arg_expr = parse_expr(exprs, i, 0)?;
                            args.push(exprs.push_expr(arg_expr));
                            if i.match_token(TokenKind::Op(Oper::Comma)).is_none() {
                                break;
                            }
                        }
                    }

                    let r_paren_token = i.expect_token(TokenKind::Op(Oper::RParen))?;
                    let span = lhs.span.join(&r_paren_token.span);
                    Expr::new(
                        ExprKind::FunctionCall {
                            func: exprs.push_expr(lhs),
                            args,
                        },
                        span,
                    )
                }
                PostfixOp::Index => {
                    i.expect_token(TokenKind::Op(Oper::RBracket))?;
                    todo!()
                }
            };
            continue;
        }

        if let Some((op, left_binding_power, right_binding_power)) = infix_op(op) {
            if left_binding_power < min_binding_power {
                break;
            }
            i.next();
            let rhs = parse_expr(exprs, i, right_binding_power)?;
            let span = lhs.span.join(&rhs.span);
            lhs = Expr::new(
                ExprKind::Binary {
                    lhs: exprs.push_expr(lhs),
                    op,
                    rhs: exprs.push_expr(rhs),
                },
                span,
            );
            continue;
        }

        break;
    }

    Ok(lhs)
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
        Oper::LBracket => Some((PostfixOp::Index, 30)),
        _ => None,
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: SourceSpan,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, span: SourceSpan) -> Self {
        Self { kind, span }
    }

    fn unexpected_token(found: &Token, expected: impl ToString) -> Self {
        Self::new(
            ParseErrorKind::UnexpectedToken {
                found: found.kind.to_string(),
                expected: expected.to_string(),
            },
            found.span,
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseErrorKind {
    UnexpectedToken { found: String, expected: String },
    UnexpectedEOF,
}

#[cfg(test)]
mod tests {
    use std::fmt::{Display, Formatter, Write};

    use crate::lexer::tokens;

    use super::*;

    fn fmt_ast(
        id: ExprId,
        expressions: &Expressions,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let expr = &expressions[id];
        match &expr.kind {
            ExprKind::Identifier(atom) => {
                write!(f, "ID {}", expressions.get_atom(*atom))
            }
            ExprKind::Number(n) => write!(f, "{}", n),
            ExprKind::Bool(v) => write!(f, "{}", v),
            ExprKind::FunctionCall { func, args } => todo!(),
            ExprKind::FunctionDef { .. } => todo!(),
            ExprKind::Unary { op, operand } => {
                write!(f, "({} ", op.as_str())?;
                fmt_ast(*operand, expressions, f)?;
                f.write_char(')')
            }
            ExprKind::Binary { lhs, op, rhs } => {
                write!(f, "({} ", op.as_str())?;
                fmt_ast(*lhs, expressions, f)?;
                f.write_char(' ')?;
                fmt_ast(*rhs, expressions, f)?;
                f.write_char(')')
            }
            ExprKind::IfThenElse { .. } => todo!(),
            ExprKind::Block { .. } => todo!(),
            ExprKind::Let { .. } => todo!(),
            ExprKind::Array(..) => todo!(),
        }
    }

    struct TestPrinter<'a>(ExprId, &'a Expressions);
    impl<'a> Display for TestPrinter<'a> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            fmt_ast(self.0, self.1, f)
        }
    }

    fn parse_and_fmt(i: &str) -> Result<String, ParseError> {
        let tokens = tokens(i).unwrap();
        let mut expressions = Expressions::new();
        let expr_id = parse(&mut expressions, &tokens)?;
        Ok(TestPrinter(expr_id, &expressions).to_string())
    }

    #[test]
    fn binary_op_parsing() {
        assert_eq!(parse_and_fmt("1 + 1"), Ok("(+ 1 1)".to_string()));
        assert_eq!(parse_and_fmt("1 -1"), Ok("(- 1 1)".to_string()));
        assert_eq!(parse_and_fmt("-1--1"), Ok("(- -1 -1)".to_string()));
        assert_eq!(parse_and_fmt("1 - 1"), Ok("(- 1 1)".to_string()));
        assert_eq!(parse_and_fmt("1 + 1 < 5"), Ok("(< (+ 1 1) 5)".to_string()));
        assert_eq!(parse_and_fmt("1 * 2 + 1"), Ok("(+ (* 1 2) 1)".to_string()));
        assert_eq!(parse_and_fmt("1 + 1 * 2"), Ok("(+ 1 (* 1 2))".to_string()));
    }
}
