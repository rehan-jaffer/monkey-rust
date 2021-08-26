#[derive(Debug, Clone)]
pub enum TokenValue {
    String(String),
    Identifier(String),
    Number(u32),
    Null
}

#[derive(Clone, Debug)]
pub struct Token {
  pub token_type: TokenType,
  pub token_literal: TokenValue
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    Asterisk,
    Assign,
    SemiColon,
    LParen,
    RParen,
    Comma,
    Plus,
    LBrace,
    RBrace,
    Ident,
    Function,
    Let,
    Unknown,
    Number,
    EOF,
    If,
    Else,
    Return,
    Eq,
    NotEq,
    Bang,
    Null,
    Minus,
    LBracket,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    True,
    False
}