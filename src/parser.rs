use super::token::{Token, TokenValue, TokenType};
use super::lexer::{Lexer};

trait Node {
  fn token_literal() -> String;
}

#[derive(Debug)]
pub struct Identifier {
//    token: Token,
    value: TokenValue
}

#[derive(Debug)]
pub enum Statement {
    LetStatement {
        token: Token,
        name: Identifier,
        value: Expression
    },
}

#[derive(Debug)]
pub struct Expression {
  
}

pub struct LetStatement {
    token: Token,
    name: Identifier,
    value: Expression
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>
}

pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    cur_token: Token,
    peek_token: Token
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Self {
        return Self {
            lexer: lexer,
            cur_token: Token { token_literal: TokenValue::Null, token_type: TokenType::Null },
            peek_token: Token { token_literal: TokenValue::Null, token_type: TokenType::Null }
        };
    }

    pub fn next_token(&mut self) {
      self.cur_token = self.peek_token.clone();
      self.peek_token = self.lexer.nextToken().clone();
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        
        let peek = self.peek_token.clone();
        
        if peek.token_type == token_type {
            self.next_token();
            return true;
        } else {
            return false;
        }

    }

    fn parse_let_statement(&mut self) -> Option<Statement> {

      if !self.expect_peek(TokenType::Ident) {
        return None;
      }

      let name =  Identifier { value: self.cur_token.token_literal.clone() };

      if !self.expect_peek(TokenType::Assign) {
          return None;
      }

      Some(Statement::LetStatement { token: Token { token_type: TokenType::Let, token_literal: TokenValue::String("let".to_string()) }, name: name, value: Expression {}  })
    }

    fn tokenizer(&mut self) -> Option<Statement> {
 
        let tok = self.cur_token.clone();
        match tok.token_type {
          TokenType::EOF => {
              return None;
          }
          TokenType::Let => {
              let stmt = self.parse_let_statement();
              return Some(stmt.unwrap());
          }
          _ => {
              return None;
          }
        }
      }
  
    pub fn parse_program(&'a mut self) -> Program {

        let lex = self.lexer.clone();
        let mut program = Program { statements: vec![] };

        loop {
            self.next_token();
            let tok = self.cur_token.clone();
            match tok.token_type {
                TokenType::EOF => {
                  break;
                },
                TokenType::Let => {
                    match self.parse_let_statement() {
                        Some(statement) => {
                            program.statements.push(statement);
                        }
                        None => {
                          
                        }
                    }
                }
                e => {
                    println!("{:?}", e);
                }
            };

        }
        return program;
    }
}