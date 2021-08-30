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
            "true" => super::token::TokenType::True,
            "false" => super::token::TokenType::False,
            _ => super::token::TokenType::Unknown
        }
      }

      fn read_digit(&mut self, first: char) -> u32 {

        let mut ident = String::new();
        ident.push(first);

        loop {
          if !self.peek_char().unwrap().is_numeric() {
                break;
          }
          ident.push(self.read_char().unwrap());
        }
        let number = ident.parse().unwrap();
        return number;
      }
  
      fn read_identifier(&mut self, first: char) -> String {

        let mut ident = String::new();
        ident.push(first);

        loop {
            match self.peek_char() {
              Some(c) => if !c.is_alphabetic() { break },
              None => { break }
            }
  
            ident.push(self.read_char().unwrap());
        }

        return ident;
      }
  
      pub fn peek_char(&mut self) -> Option<&char> {

          return self.input.peek();
  
      }
  
      pub fn read_char(&mut self) -> Option<char> {
  
        return self.input.next();

      }
  
      fn skip_whitespace(&mut self) {
        let whitespace = [' ', '\t', '\r', '\n'];
        loop {
          match self.peek_char() {
            Some(c) => {
              if !whitespace.contains(c) {
                break;
              }    
            }
            None => {
              break;
            }
          }
          self.read_char();
        } 
      }

  
      pub fn nextToken(&mut self) -> super::token::Token {
  
        self.skip_whitespace();
  
        return match self.read_char() {
            Some('=') => {
                match self.peek_char() {
                    Some('=') => {
                      self.read_char();
                      return super::token::Token { token_type: super::token::TokenType::Eq, token_literal: super::token::TokenValue::String("==".to_string()) }
                    },
                    _ => {
                      return super::token::Token { token_type: super::token::TokenType::Assign, token_literal: super::token::TokenValue::String("=".to_string()) }
                    }
                };
              },
              Some('!') => {
                  match self.peek_char() {
                      Some('=') => {
                        self.read_char();
                        return super::token::Token { token_type: super::token::TokenType::NotEq, token_literal: super::token::TokenValue::String("!=".to_string()) }
                      },
                      _ => {
                        return super::token::Token { token_type: super::token::TokenType::Bang, token_literal: super::token::TokenValue::String("!".to_string()) }
                      }
                  };
                },
            Some('{') => super::token::Token { token_type: super::token::TokenType::LBrace, token_literal: super::token::TokenValue::String("{".to_string()) },
            Some('}') => super::token::Token { token_type: super::token::TokenType::RBrace, token_literal: super::token::TokenValue::String("}".to_string()) },    
            Some(';') => super::token::Token { token_type: super::token::TokenType::SemiColon, token_literal: super::token::TokenValue::String(";".to_string()) },
            Some('(') => super::token::Token { token_type: super::token::TokenType::LParen, token_literal: super::token::TokenValue::String("(".to_string()) },
            Some(')') => super::token::Token { token_type: super::token::TokenType::RParen, token_literal: super::token::TokenValue::String(")".to_string()) },
            Some(',') => super::token::Token { token_type: super::token::TokenType::Comma, token_literal: super::token::TokenValue::String(",".to_string()) },
            Some('-') => super::token::Token { token_type: super::token::TokenType::Minus, token_literal: super::token::TokenValue::String("-".to_string()) },
            Some('+') => super::token::Token { token_type: super::token::TokenType::Plus, token_literal: super::token::TokenValue::String("+".to_string()) },
            Some('*') => super::token::Token { token_type: super::token::TokenType::Asterisk, token_literal: super::token::TokenValue::String("]".to_string()) },
            Some('/') => super::token::Token { token_type: super::token::TokenType::Slash, token_literal: super::token::TokenValue::String("/".to_string()) },
            Some('<') => super::token::Token { token_type: super::token::TokenType::LessThan, token_literal: super::token::TokenValue::String("<".to_string()) },
            Some('>') => super::token::Token { token_type: super::token::TokenType::GreaterThan, token_literal: super::token::TokenValue::String(">".to_string()) },
            None => super::token::Token { token_type: super::token::TokenType::EOF, token_literal: super::token::TokenValue::String("\0".to_string()) },
            Some(e) => {
              if Lexer::is_digit(e) {
                return super::token::Token { token_type: super::token::TokenType::Number, token_literal: super::token::TokenValue::Number(self.read_digit(e)) }
              }
  
              let string = self.read_identifier(e);
  
                if Lexer::is_keyword(string.as_str()) {
                    return super::token::Token { token_type: Lexer::keyword(string.as_str()), token_literal: super::token::TokenValue::String(string) }
                } else {
                    return super::token::Token { token_type: super::token::TokenType::Ident, token_literal: super::token::TokenValue::Identifier(string) }
                }
            }
  
        };
  
      }
  }
  