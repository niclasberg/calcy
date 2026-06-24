use std::{fmt::Display, ops::Range};

use winnow::{
    Parser, Result,
    ascii::{float, multispace0},
    combinator::{alt, dispatch, empty, fail},
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
    Eof,
}

impl Display for TokenKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Literal(n) => Display::fmt(n, f),
            TokenKind::Identifier(name) => f.write_str(name),
            TokenKind::Keyword(keyword) => f.write_str(keyword.as_str()),
            TokenKind::Op(oper) => f.write_str(oper.as_str()),
            TokenKind::Eof => f.write_str("EOF"),
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
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Colon,
    SemiColon, // ;
    Comma,     // ,
    Arrow,     // =>
    BinaryOr,  // |
    Spread,    // ..
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
            Oper::LBracket => "[",
            Oper::RBracket => "]",
            Oper::LBrace => "{",
            Oper::RBrace => "}",
            Oper::SemiColon => ";",
            Oper::Colon => ":",
            Oper::Comma => ",",
            Oper::Arrow => "=>",
            Oper::BinaryOr => "|",
            Oper::Spread => "..",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    If,
    Then,
    Else,
    Fn,
    Let,
    Int,
    Float,
    Bool,
}

impl Keyword {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::Then => "then",
            Keyword::Fn => "fn",
            Keyword::Let => "let",
            Keyword::Int => "Int",
            Keyword::Float => "Float",
            Keyword::Bool => "Bool",
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
    let mut tokens = Vec::new();
    let mut slice = i;
    while !slice.is_empty() {
        multispace0(&mut slice)?;
        if !slice.is_empty() {
            let start = i.len() - slice.len();
            let token = token(&mut slice)?;
            let end = i.len() - slice.len();
            tokens.push(Token {
                kind: token,
                span: SourceSpan { start, end },
            });
        }
    }
    Ok(tokens)
}

fn token<'s>(i: &mut &'s str) -> Result<TokenKind<'s>> {
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
            '.' => alt((
                '.'.value(Oper::Spread),
                empty.value(Oper::Dot)
            )),
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
                '>'.value(Oper::Arrow),
                empty.value(Oper::Assign)
            )),
            '[' => empty.value(Oper::LBracket),
            ']' => empty.value(Oper::RBracket),
            '{' => empty.value(Oper::LBrace),
            '}' => empty.value(Oper::RBrace),
            ':' => empty.value(Oper::Colon),
            ';' => empty.value(Oper::SemiColon),
            ',' => empty.value(Oper::Comma),
            '|' => empty.value(Oper::BinaryOr),
            _ => fail
        }
        .map(TokenKind::Op),
    ))
    .parse_next(i)
}

fn identifier_or_keyword<'s>(i: &mut &'s str) -> Result<TokenKind<'s>> {
    (
        one_of(|c: char| c.is_alpha() || c == '_'),
        take_while(0.., |c: char| c.is_alphanum() || c == '_'),
    )
        .take()
        .map(|id| match id {
            "if" => TokenKind::Keyword(Keyword::If),
            "else" => TokenKind::Keyword(Keyword::Else),
            "then" => TokenKind::Keyword(Keyword::Then),
            "fn" => TokenKind::Keyword(Keyword::Fn),
            "let" => TokenKind::Keyword(Keyword::Let),
            "true" => TokenKind::Literal(Literal::Bool(true)),
            "false" => TokenKind::Literal(Literal::Bool(false)),
            "Int" => TokenKind::Keyword(Keyword::Int),
            "Float" => TokenKind::Keyword(Keyword::Float),
            "Bool" => TokenKind::Keyword(Keyword::Bool),
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
