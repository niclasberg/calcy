use std::{fmt::Display, ops::Range};

use winnow::{
    LocatingSlice, Parser, Result,
    ascii::{float, multispace0},
    combinator::{alt, delimited, dispatch, empty, eof, fail, repeat},
    error::ContextError,
    stream::{AsChar, TokenSlice},
    token::{any, one_of, take_while},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'s> {
    pub kind: TokenKind<'s>,
    pub span: SourceSpan,
}

pub type Tokens<'i> = TokenSlice<'i, Token<'i>>;

impl<'s> PartialEq<TokenKind<'s>> for Token<'s> {
    fn eq(&self, other: &TokenKind<'s>) -> bool {
        self.kind == *other
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind<'s> {
    Number(f64),
    Identifier(&'s str),
    Keyword(Keyword),
    LParen,
    RParen,
    Op(Oper),
}

impl Display for TokenKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Number(n) => Display::fmt(n, f),
            TokenKind::Identifier(name) => f.write_str(name),
            TokenKind::Keyword(keyword) => f.write_str(keyword.as_str()),
            TokenKind::LParen => f.write_str("("),
            TokenKind::RParen => f.write_str(")"),
            TokenKind::Op(oper) => f.write_str(oper.as_str()),
        }
    }
}

impl<'i> Parser<Tokens<'i>, &'i Token<'i>, ContextError> for TokenKind<'i> {
    fn parse_next(&mut self, input: &mut Tokens<'i>) -> Result<&'i Token<'i>, ContextError> {
        any.verify(|token: &Token<'i>| token.kind == *self)
            .parse_next(input)
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
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    If,
    Else,
    Fn,
    True,
    False,
}

impl Keyword {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::Fn => "fn",
            Keyword::True => "true",
            Keyword::False => "false",
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

pub fn tokens<'s>(i: &'s str) -> Result<Vec<Token<'s>>> {
    let (tokens, _) = (repeat(1.., delimited(multispace0, token, multispace0)), eof)
        .parse_next(&mut LocatingSlice::new(i))?;
    Ok(tokens)
}

fn token<'s>(i: &mut LocatingSlice<&'s str>) -> Result<Token<'s>> {
    alt((
        identifier_or_keyword,
        float.map(TokenKind::Number),
        dispatch! {any;
            '(' => empty.value(TokenKind::LParen),
            ')' => empty.value(TokenKind::RParen),
            '+' => empty.value(TokenKind::Op(Oper::Add)),
            '-' => empty.value(TokenKind::Op(Oper::Sub)),
            '*' => empty.value(TokenKind::Op(Oper::Mul)),
            '/' => empty.value(TokenKind::Op(Oper::Div)),
            '.' => empty.value(TokenKind::Op(Oper::Dot)),
            '<' => alt((
                '='.value(TokenKind::Op(Oper::LessEq)),
                empty.value(TokenKind::Op(Oper::Less))
            )),
            '>' => alt((
                '='.value(TokenKind::Op(Oper::GreaterEq)),
                empty.value(TokenKind::Op(Oper::Greater))
            )),
            '!' => alt((
                '='.value(TokenKind::Op(Oper::NotEq)),
                empty.value(TokenKind::Op(Oper::Not))
            )),
            '=' => alt((
                '='.value(TokenKind::Op(Oper::Eq)),
                empty.value(TokenKind::Op(Oper::Assign))
            )),
            _ => fail
        },
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
            "true" => TokenKind::Keyword(Keyword::True),
            "false" => TokenKind::Keyword(Keyword::False),
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
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::Op(Oper::Add),
            TokenKind::Op(Oper::Sub),
            TokenKind::Keyword(Keyword::If),
            TokenKind::Op(Oper::Sub),
            TokenKind::Number(-10.0),
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
        let mut i = input;
        let tokens = tokens(&mut i).unwrap();
        let token_kinds: Vec<_> = tokens.into_iter().map(|token| token.kind).collect();
        assert_eq!(token_kinds, expected_tokens);
    }
}
