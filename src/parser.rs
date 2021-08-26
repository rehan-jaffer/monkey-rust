use super::token::{Token, TokenValue, TokenType};
use super::lexer::{Lexer};
use std::sync::{Arc, Mutex};

#[derive(Debug)]
enum ParserError {
    UnexpectedTokenError(TokenType, TokenType),
    UnknownTokenError(TokenType)
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
enum Precedence {
  LOWEST = 0,
  EQUALS = 1,
  LESSGREATER = 2,
  SUM = 3,
  PRODUCT = 4,
  PREFIX = 5,
  CALL = 6
}

#[derive(Debug)]
pub struct Identifier {
//    token: Token,
    value: TokenValue
}

#[derive(Debug)]
pub enum Node {
  Statement(Statement),
  Expression(Expr)
}

impl Expr {
    pub fn serialize(&self) -> String {
 
        let mut ret = String::new();
 
        match self {
            Expr::Ident(ident) => {
              ret += "()";
            },
            Expr::Literal(literal) => {
                ret += " ";
                ret += literal.serialize().as_str();
            },
            Expr::Infix(infix, expr1, expr2) => {
                ret += " (";

                ret += match infix {
                  Infix::Plus => "+",
                  Infix::Minus => "-",
                  Infix::Divide => "/",
                  Infix::Multiply => "*",
                  Infix::Equal => "==",
                  Infix::NotEqual => "!=",
                  Infix::GreaterThan => ">",
                  Infix::GreaterThanEqual => ">=",
                  Infix::LessThan => "<",
                  Infix::LessThanEqual => "<=",
                  Infix::Null => "?"
              };

              ret += expr1.serialize().as_str();
              ret += expr2.serialize().as_str();
              ret += ")";
            },
            Expr::Prefix(prefix, expr) => {
                ret += " ";
                match prefix {
                  Prefix::Minus => {
                      ret += "(-";
                      ret += expr.serialize().as_str();
                      ret += ")";
                  },
                  Prefix::Bang => {
                    ret += "(!";
                    ret += expr.serialize().as_str();
                    ret += ")";
                  }
                  _ => {}
                }
            },
            Null => {}
        }
        return ret;
    }
}

impl Node {
  pub fn serialize(&self) -> String {

      let mut ret = String::new();
      ret += "(";

      match self {
          Node::Statement(statement) => {
            match statement {
              Statement::Let(Ident(TokenValue::Identifier(ident)), expr) => {
                ret += "let ";
                ret += ident;
                ret += expr.serialize().as_str();
              },
              _ => {
                  ret += "MISSING"
              }
            };
          }
          Node::Expression(expression) => {
            ret += "unimplemented";
          }
      }

      ret += ")";
      return ret;
    }

}

#[derive(Debug)]
pub struct Ident(pub TokenValue);

#[derive(PartialEq, Clone, Debug)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    GreaterThanEqual,
    GreaterThan,
    LessThanEqual,
    LessThan,
    Null
}

#[derive(Debug, Clone)]
pub enum Prefix {
    Bang,
    Plus,
    Minus
}

#[derive(Debug)]
pub enum Statement {
  Null,
  Let(Ident, Expr),
  Return(Expr),
  Expr(Expr)
}

pub type Block = Vec<Node>;

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    Int(u32),
    String(String),
    Bool(bool),
}

impl Literal {
    fn serialize(&self) -> String {
      
      let mut ret = String::new();

      match self {
          Literal::Int(i) => {
              ret += i.to_string().as_str();
          },
          Literal::String(s) => {
            ret += s;
          },
          Literal::Bool(b) => {
            ret += b.to_string().as_str();
          }
      }

      return ret;
    }
}

#[derive(Debug)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Infix(Infix, Box<Expr>, Box<Expr>),
    Prefix(Prefix, Box<Expr>),
    Null
}

impl Expr {
}

#[derive(Debug)]
pub enum Expression {
  PrefixExpression,
  InfixExpression,
  PostfixExpression,
  NullExpression
}

pub struct LetStatement {
    token: Token,
    name: Identifier,
    value: Expression
}

#[derive(Debug)]
pub struct Program {
    pub statements: Block
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

    fn load_first_tokens(&mut self) {
        self.next_token();
        self.next_token();
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

    fn parse_expression(&mut self) -> Expression {

        return Expression::NullExpression;

    }

    fn parse_ident_expr(&mut self) -> Result<Expr, ParserError> {
      Ok(Expr::Null)
    }

    fn parse_number(&mut self) -> Result<Expr, ParserError> {
      match self.cur_token {
        Token { token_type: _, token_literal: TokenValue::Number(i) } => Ok(Expr::Literal(Literal::Int(i))),
        _ => { Err(ParserError::UnknownTokenError(self.cur_token.token_type.clone())) }
      }
    }

    fn token_precedence(&mut self, token_type: TokenType) -> Precedence {
      match token_type {
          TokenType::Equal | TokenType::NotEqual => Precedence::EQUALS,
          TokenType::Plus | TokenType::Minus => Precedence::SUM,
          _ => Precedence::LOWEST,
      }
    }

    fn parse_infix_expr(&mut self, left: Expr) -> Result<Expr, ParserError> {
        let infix = match self.cur_token.token_type.clone() {
          TokenType::Plus => Infix::Plus,
          TokenType::Minus => Infix::Minus,
          _ => Infix::Null
        };

        let precedence = self.token_precedence(self.cur_token.token_type.clone());
        self.next_token();

        match self.parse_expr(precedence) {
            Ok(expr) => Ok(Expr::Infix(infix, Box::new(left), Box::new(expr))),
            Err(e) => Err(e)
        }
    }

    fn parse_prefix_expr(&mut self) -> Result<Expr, ParserError> {

      let prefix = match self.cur_token.token_type {
          TokenType::Bang => Prefix::Bang,
          TokenType::Plus => Prefix::Plus,
          TokenType::Minus => Prefix::Minus,
          _ => return Err(ParserError::UnknownTokenError(self.cur_token.token_type.clone()))
      };

      self.next_token();

      return match self.parse_expr(Precedence::PREFIX) {
          Ok(e) => Ok(Expr::Prefix(prefix, Box::new(e))),
          Err(e) => Err(e)
      };

    }

    fn parse_expr(&mut self, precedence: Precedence) -> Result<Expr, ParserError> {

      let mut lhs = match self.cur_token.token_type {
        TokenType::Ident => self.parse_ident_expr(),
        TokenType::Number => self.parse_number(),
        TokenType::Bang | TokenType::Minus | TokenType::Plus => self.parse_prefix_expr(),
        _ => {
            println!("Error in parse expression!");
            return Err(ParserError::UnknownTokenError(self.cur_token.token_type.clone()));
        }
      }.unwrap();

      while self.peek_token.token_type != TokenType::SemiColon && (precedence < self.token_precedence(self.peek_token.token_type.clone())) {

        println!("{:?}", self.peek_token.token_type);

        match self.peek_token.token_type {
        TokenType::Plus
        | TokenType::Minus => {
            self.next_token();
            lhs = self.parse_infix_expr(lhs).unwrap();
        }
        TokenType::LBracket => {
            self.next_token();
        }
        TokenType::LParen => {
            self.next_token();
        }
        _ => return Ok(lhs),
      }

    }

      return Ok(lhs)

    }

    fn parse_let_statement(&mut self) -> Result<Node, ParserError> {

      if !self.expect_peek(TokenType::Ident) {
        return Err(ParserError::UnexpectedTokenError(TokenType::Ident, self.peek_token.token_type.clone()));
      }

      let name =  self.cur_token.token_literal.clone();

      if !self.expect_peek(TokenType::Assign) {
        return Err(ParserError::UnexpectedTokenError(TokenType::Assign, self.peek_token.token_type.clone()));
      }

      self.next_token();

      let expr = match self.parse_expr(Precedence::LOWEST) {
        Ok(expr) => expr,
        Err(e) => return Err(e),
      };

      if !self.expect_peek(TokenType::SemiColon) {
        return Err(ParserError::UnexpectedTokenError(TokenType::SemiColon, self.peek_token.token_type.clone()));
      }

      Ok(Node::Statement(Statement::Let(Ident(name), expr)))
    }

      fn parse_statement(&mut self) -> Result<Node, ParserError> {


        match self.cur_token.token_type {
          TokenType::Let => { self.parse_let_statement() },
          TokenType::Null => { Ok(Node::Statement(Statement::Null)) }
          _ => {
            Err(ParserError::UnknownTokenError(self.cur_token.token_type.clone()))
          }
        }
      }
  
    pub fn parse_program(&'a mut self) -> Program {

        let mut program = Program { statements: vec![] };
        self.load_first_tokens();

        loop {

            if self.cur_token.token_type == TokenType::EOF {
              break;
            }

            match self.parse_statement() {
                Ok(statement) => {
                    program.statements.push(statement)
                }
                Err(e) => { panic!("Parser error! {:?}", e) }
            };

            self.next_token();

        }
        return program;
    }

}
