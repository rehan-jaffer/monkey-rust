use std::{io, io::prelude::*};
use std::str::Chars;
use std::iter::Peekable;

const PROMPT : &str = ">>";

pub struct REPL {

}

impl REPL {
  pub fn start(&self) {

    let stdin = io::stdin();
    
    for line in stdin.lock().lines() {
        let input = line.unwrap();
        let mut lexer = super::lexer::Lexer { input: input.chars().peekable() };
        loop {
            let tok = lexer.nextToken();
            match tok.token_type {
                super::token::TokenType::EOF => {
                    break;
                }
                _ => {
                    println!("<TOKEN::{:?} {:?}>", tok.token_type, tok.token_literal);
                }
            }
        }
    }

  }
}