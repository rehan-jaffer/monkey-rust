use std::{io, io::prelude::*};

const PROMPT : &str = ">>";

pub struct REPL {

}

impl REPL {
  pub fn start(&self) {

    let stdin = io::stdin();
    
    for line in stdin.lock().lines() {
        let mut lexer = super::lexer::Lexer { read_position: 0, position: 0, char: '\0', input: line.unwrap() };
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