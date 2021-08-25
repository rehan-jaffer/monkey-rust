use super::token::{Token};
use std::str::Chars;
use std::iter::Peekable;

#[derive(Clone)]
pub struct Lexer<'a> {
    pub input: Peekable<Chars<'a>>,
  }
  
  impl<'a> Lexer<'a> {
  
      pub fn is_digit(possible_digit: char) -> bool {
        return possible_digit.is_numeric();
      }
  
      pub fn is_keyword(keyword: &str) -> bool {
        return match Lexer::keyword(keyword) {
            super::token::TokenType::Unknown => false,
            _ => true
        }
      }
  
      pub fn keyword(keyword: &str) -> super::token::TokenType {
        return match keyword {
            "fn" => super::token::TokenType::Function,
            "let" => super::token::TokenType::Let,
            "if" => super::token::TokenType::If,
            "else" => super::token::TokenType::Else,
            "return" => super::token::TokenType::Return,
            _ => super::token::TokenType::Unknown
        }
      }

      fn readDigit(&mut self, first: char) -> u32 {

        let mut ident = String::new();
        ident.push(first);

        loop {
          if !self.peekChar().unwrap().is_numeric() {
                break;
          }
          ident.push(self.readChar().unwrap());
        }
        let number = ident.parse().unwrap();
        return number;
      }
  
      fn readIdentifier(&mut self, first: char) -> String {

        let mut ident = String::new();
        ident.push(first);

        loop {
            if !self.peekChar().unwrap().is_alphabetic() {
                break;
            }
  
            ident.push(self.readChar().unwrap());
        }

        return ident;
      }
  
      pub fn peekChar(&mut self) -> Option<&char> {

          return self.input.peek();
  
      }
  
      pub fn readChar(&mut self) -> Option<char> {
  
        return self.input.next();

      }
  
      fn skip_whitespace(&mut self) {
        let whitespace = [' ', '\t', '\r', '\n'];
        loop {
          match self.peekChar() {
            Some(c) => {
              if !whitespace.contains(c) {
                break;
              }    
            }
            None => {
              break;
            }
          }
          self.readChar();
        } 
      }

  
      pub fn nextToken(&mut self) -> super::token::Token {
  
        self.skip_whitespace();
  
        return match self.readChar() {
            Some('=') => {
                match self.peekChar() {
                    Some('=') => {
                      self.readChar();
                      return super::token::Token { token_type: super::token::TokenType::Eq, token_literal: super::token::TokenValue::String("==".to_string()) }
                    },
                    _ => {
                      return super::token::Token { token_type: super::token::TokenType::Assign, token_literal: super::token::TokenValue::String("=".to_string()) }
                    }
                };
              },
              Some('!') => {
                  match self.peekChar() {
                      Some('=') => {
                        self.readChar();
                        return super::token::Token { token_type: super::token::TokenType::NotEq, token_literal: super::token::TokenValue::String("!=".to_string()) }
                      },
                      _ => {
                        return super::token::Token { token_type: super::token::TokenType::Bang, token_literal: super::token::TokenValue::String("!".to_string()) }
                      }
                  };
                },
            Some(';') => super::token::Token { token_type: super::token::TokenType::SemiColon, token_literal: super::token::TokenValue::String(";".to_string()) },
            Some('(') => super::token::Token { token_type: super::token::TokenType::LParen, token_literal: super::token::TokenValue::String("(".to_string()) },
            Some(')') => super::token::Token { token_type: super::token::TokenType::RParen, token_literal: super::token::TokenValue::String(")".to_string()) },
            Some(',') => super::token::Token { token_type: super::token::TokenType::Comma, token_literal: super::token::TokenValue::String(",".to_string()) },
            Some('-') => super::token::Token { token_type: super::token::TokenType::Minus, token_literal: super::token::TokenValue::String("-".to_string()) },
            Some('+') => super::token::Token { token_type: super::token::TokenType::Plus, token_literal: super::token::TokenValue::String("+".to_string()) },
            Some('[') => super::token::Token { token_type: super::token::TokenType::LBrace, token_literal: super::token::TokenValue::String("[".to_string()) },
            Some(']') => super::token::Token { token_type: super::token::TokenType::RBrace, token_literal: super::token::TokenValue::String("]".to_string()) },
            None => super::token::Token { token_type: super::token::TokenType::EOF, token_literal: super::token::TokenValue::String("\0".to_string()) },
            Some(e) => {
              if Lexer::is_digit(e) {
                return super::token::Token { token_type: super::token::TokenType::Number, token_literal: super::token::TokenValue::Number(self.readDigit(e)) }
              }
  
              let string = self.readIdentifier(e);
  
                if Lexer::is_keyword(string.as_str()) {
                    return super::token::Token { token_type: Lexer::keyword(string.as_str()), token_literal: super::token::TokenValue::String(string) }
                } else {
                    return super::token::Token { token_type: super::token::TokenType::Ident, token_literal: super::token::TokenValue::Identifier(string) }
                }
            }
  
        };
  
      }
  }
  