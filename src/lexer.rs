use std::{fmt::Display, ops::Range};

use winnow::{
    LocatingSlice, Parser, Result,
    ascii::{float, multispace0},
    combinator::{alt, delimited, dispatch, empty, eof, fail, repeat},
    stream::AsChar,
    token::{any, one_of, take_while},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'s> {
    pub kind: TokenKind<'s>,
    pub span: SourceSpan,
}

impl<'s> PartialEq<TokenKind<'s>> for Token<'s> {
    fn eq(&self, other: &TokenKind<'s>) -> bool {
        self.kind == *other
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind<'s> {
    Identifier(&'s str),
    Literal(Literal),
    Keyword(Keyword),
    Op(Oper),
}

impl Display for TokenKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Literal(n) => Display::fmt(n, f),
            TokenKind::Identifier(name) => f.write_str(name),
            TokenKind::Keyword(keyword) => f.write_str(keyword.as_str()),
            TokenKind::Op(oper) => f.write_str(oper.as_str()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal {
    Number(f64),
    Bool(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(value) => Display::fmt(value, f),
            Literal::Bool(value) => Display::fmt(value, f),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Oper {
    Add,
    Sub,
    Mul,
    Div,
    Dot,
    Not,
    Assign,
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    LParen,
    RParen,
    LCurlyBrace,
    RCurlyBrace,
    LBrace,
    RBrace,
    SemiColon,
    Comma,
}

impl Oper {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Oper::Add => "+",
            Oper::Sub => "-",
            Oper::Mul => "*",
            Oper::Div => "/",
            Oper::Dot => ".",
            Oper::Not => "!",
            Oper::Assign => "=",
            Oper::Eq => "==",
            Oper::NotEq => "!=",
            Oper::Less => "<",
            Oper::LessEq => "<=",
            Oper::Greater => ">",
            Oper::GreaterEq => ">=",
            Oper::LParen => "(",
            Oper::RParen => ")",
            Oper::LCurlyBrace => "[",
            Oper::RCurlyBrace => "]",
            Oper::LBrace => "{",
            Oper::RBrace => "}",
            Oper::SemiColon => ";",
            Oper::Comma => ",",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    If,
    Else,
    Fn,
}

impl Keyword {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::Fn => "fn",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
}

impl SourceSpan {
    pub fn join(&self, other: &Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl From<Range<usize>> for SourceSpan {
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

impl ariadne::Span for SourceSpan {
    type SourceId = ();

    fn source(&self) -> &Self::SourceId {
        &()
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

pub fn tokens<'s>(i: &'s str) -> Result<Vec<Token<'s>>> {
    let (tokens, _) = (repeat(1.., delimited(multispace0, token, multispace0)), eof)
        .parse_next(&mut LocatingSlice::new(i))?;
    Ok(tokens)
}

fn token<'s>(i: &mut LocatingSlice<&'s str>) -> Result<Token<'s>> {
    alt((
        identifier_or_keyword,
        float.map(|value| TokenKind::Literal(Literal::Number(value))),
        dispatch! {any;
            '(' => empty.value(Oper::LParen),
            ')' => empty.value(Oper::RParen),
            '+' => empty.value(Oper::Add),
            '-' => empty.value(Oper::Sub),
            '*' => empty.value(Oper::Mul),
            '/' => empty.value(Oper::Div),
            '.' => empty.value(Oper::Dot),
            '<' => alt((
                '='.value(Oper::LessEq),
                empty.value(Oper::Less)
            )),
            '>' => alt((
                '='.value(Oper::GreaterEq),
                empty.value(Oper::Greater)
            )),
            '!' => alt((
                '='.value(Oper::NotEq),
                empty.value(Oper::Not)
            )),
            '=' => alt((
                '='.value(Oper::Eq),
                empty.value(Oper::Assign)
            )),
            '[' => empty.value(Oper::LBrace),
            ']' => empty.value(Oper::RBrace),
            '{' => empty.value(Oper::LCurlyBrace),
            '}' => empty.value(Oper::RCurlyBrace),
            ';' => empty.value(Oper::SemiColon),
            ',' => empty.value(Oper::Comma),
            _ => fail
        }
        .map(TokenKind::Op),
    ))
    .with_span()
    .map(|(kind, span)| Token {
        kind,
        span: span.into(),
    })
    .parse_next(i)
}

fn identifier_or_keyword<'s>(i: &mut LocatingSlice<&'s str>) -> Result<TokenKind<'s>> {
    (
        one_of(|c: char| c.is_alpha() || c == '_'),
        take_while(0.., |c: char| c.is_alphanum() || c == '_'),
    )
        .take()
        .map(|id| match id {
            "if" => TokenKind::Keyword(Keyword::If),
            "else" => TokenKind::Keyword(Keyword::Else),
            "fn" => TokenKind::Keyword(Keyword::Fn),
            "true" => TokenKind::Literal(Literal::Bool(true)),
            "false" => TokenKind::Literal(Literal::Bool(false)),
            _ => TokenKind::Identifier(id),
        })
        .parse_next(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_tokens() {
        let input = "( )\t+\n- if --10 else */ <=<<=>>=fn banana";
        let expected_tokens = vec![
            TokenKind::Op(Oper::LParen),
            TokenKind::Op(Oper::RParen),
            TokenKind::Op(Oper::Add),
            TokenKind::Op(Oper::Sub),
            TokenKind::Keyword(Keyword::If),
            TokenKind::Op(Oper::Sub),
            TokenKind::Literal(Literal::Number(-10.0)),
            TokenKind::Keyword(Keyword::Else),
            TokenKind::Op(Oper::Mul),
            TokenKind::Op(Oper::Div),
            TokenKind::Op(Oper::LessEq),
            TokenKind::Op(Oper::Less),
            TokenKind::Op(Oper::LessEq),
            TokenKind::Op(Oper::Greater),
            TokenKind::Op(Oper::GreaterEq),
            TokenKind::Keyword(Keyword::Fn),
            TokenKind::Identifier("banana"),
        ];
        let i = input;
        let tokens = tokens(i).unwrap();
        let token_kinds: Vec<_> = tokens.into_iter().map(|token| token.kind).collect();
        assert_eq!(token_kinds, expected_tokens);
    }
}
