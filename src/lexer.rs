pub struct Lexer {
    pub input: String,
    pub position: u32,
    pub read_position: u32,
    pub char: char
  }
  
  impl Lexer {
  
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
  
      fn readDigit(&mut self) -> u32 {
          let pos = self.position;
          loop {
              if !self.char.is_numeric() {
                  break;
              }
    
              self.readChar();
          }
          let string = &self.input.as_str()[pos as usize..self.position as usize];
          let number = string.parse().unwrap();
          return number;
      }
  
      fn readIdentifier(&mut self) -> &str {
        let pos = self.position;
        loop {
            if !self.char.is_alphabetic() {
                break;
            }
  
            self.readChar();
        }
        return &self.input.as_str()[pos as usize..self.position as usize];
      }
  
      pub fn peekChar(&mut self) -> Option<char> {
          if self.read_position >= self.input.len() as u32 {
              return None;
          } else {
            return Some(self.input.chars().nth(self.read_position as usize).unwrap())
          }
  
      }
  
      pub fn readChar(&mut self) {
  
        if self.read_position >= self.input.len() as u32 {
            self.char = '\0';
        } else {
            self.char = self.input.chars().nth(self.read_position as usize).unwrap();
        }
  
        self.position = self.read_position;
        self.read_position += 1;
      }
  
      fn skip_whitespace(&mut self) {
        let whitespace = [' ', '\t', '\r', '\n'];
        loop {
          if !whitespace.contains(&self.char) {
            break;
          }
          self.readChar();
        } 
      }
  
      pub fn nextToken(&mut self) -> super::token::Token {
  
          self.readChar();
          self.skip_whitespace();
  
        return match self.char {
            '=' => {
                match self.peekChar() {
                    Some('=') => {
                      self.readChar();
                      return super::token::Token { token_type: super::token::TokenType::Eq, token_literal: super::token::TokenValue::String("==") }
                    },
                    _ => {
                      return super::token::Token { token_type: super::token::TokenType::Assign, token_literal: super::token::TokenValue::String("=") }
                    }
                };
              },
              '!' => {
                  match self.peekChar() {
                      Some('=') => {
                        self.readChar();
                        return super::token::Token { token_type: super::token::TokenType::NotEq, token_literal: super::token::TokenValue::String("!=") }
                      },
                      _ => {
                        return super::token::Token { token_type: super::token::TokenType::Bang, token_literal: super::token::TokenValue::String("!") }
                      }
                  };
                },
              ';' => super::token::Token { token_type: super::token::TokenType::SemiColon, token_literal: super::token::TokenValue::String(";") },
            '(' => super::token::Token { token_type: super::token::TokenType::LParen, token_literal: super::token::TokenValue::String("(") },
            ')' => super::token::Token { token_type: super::token::TokenType::RParen, token_literal: super::token::TokenValue::String(")") },
            ',' => super::token::Token { token_type: super::token::TokenType::Comma, token_literal: super::token::TokenValue::String(",") },
            '+' => super::token::Token { token_type: super::token::TokenType::Plus, token_literal: super::token::TokenValue::String("+") },
            '[' => super::token::Token { token_type: super::token::TokenType::LBrace, token_literal: super::token::TokenValue::String("[") },
            ']' => super::token::Token { token_type: super::token::TokenType::RBrace, token_literal: super::token::TokenValue::String("]") },
            '\0' => super::token::Token { token_type: super::token::TokenType::EOF, token_literal: super::token::TokenValue::String("\0") },
            e => {
              if Lexer::is_digit(e) {
                return super::token::Token { token_type: super::token::TokenType::Number, token_literal: super::token::TokenValue::Number(self.readDigit()) }
              }
  
              let string = self.readIdentifier();
  
                if Lexer::is_keyword(string) {
                    return super::token::Token { token_type: Lexer::keyword(string), token_literal: super::token::TokenValue::String(string) }
                } else {
                    return super::token::Token { token_type: super::token::TokenType::Ident, token_literal: super::token::TokenValue::Identifier(string) }
                }
            }
  
        };
  
      }
  }
  