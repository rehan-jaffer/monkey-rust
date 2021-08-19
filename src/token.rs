#[derive(Debug)]
pub enum TokenValue<'a> {
    String(&'a str),
    Identifier(&'a str),
    Number(u32)
}

pub struct Token<'a> {
  pub token_type: TokenType,
  pub token_literal: TokenValue<'a>
}

#[derive(Debug)]
pub enum TokenType {
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
    Bang
}