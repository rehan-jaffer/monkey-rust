use super::token::{Token, TokenValue, TokenType};
use super::lexer::{Lexer};
use std::sync::{Arc, Mutex};

#[derive(Debug)]
enum ParserError {
    UnexpectedTokenError(TokenType, TokenType, String),
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
            Expr::Ident(Ident(ident)) => {
                match ident {
                    TokenValue::Identifier(str) => ret += format!(" {}", str).as_str(),
                    _ => { panic!("") }
                }
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
            Expr::Conditional(Conditional { condition, consequence, alternative }) => {

                ret += format!("({}) ? ", condition.serialize()).as_str();

                for statement in consequence {
                    ret += format!("({})", statement.serialize().as_str()).as_str();
                }

                ret += " : ";

                match alternative {
                    None => {
                        ret += "()"
                    }
                    Some(alternative) => {
                        for statement in alternative {
                            ret += format!("({})", statement.serialize().as_str()).as_str();
                        }        
                    }
                }

                ret += "";

            },
            _ => {}
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
            let stmt = match statement {
              Statement::Let(Ident(TokenValue::Identifier(ident)), expr) => { format!("{} {}{}", statement.serialize(), ident, &expr.serialize()) }
              Statement::Return(expr) => { format!("{}{}", statement.serialize(), &expr.serialize()) },
              Statement::Expr(expr) => { expr.serialize().to_string() }
              _ => {
                  "MISSING".to_string()
              }
            };
            ret += stmt.as_str();
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

impl Statement {
    pub fn serialize(&self) -> &str {
        match self {
            Statement::Let(_, _) => "let",
            Statement::Return(_) => "ret",
            _ => ""
        }
    }
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
pub struct Conditional {
    condition: Box<Expr>,
    consequence: Block,
    alternative: Option<Block>
}

#[derive(Debug)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Infix(Infix, Box<Expr>, Box<Expr>),
    Prefix(Prefix, Box<Expr>),
    Conditional(Conditional),
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
      return Ok(Expr::Ident(Ident(self.cur_token.token_literal.clone())))
    }

    fn current_token_is(&self, token_type: TokenType) -> bool {
      return (self.cur_token.token_type.clone() == token_type);
    }

    fn peek_token_is(&self, token_type: TokenType) -> bool {
        return (self.peek_token.token_type.clone() == token_type);
    }  

    fn parse_block_statement(&mut self) -> Result<Block, ParserError> {

        let mut block = Vec::new();

        self.next_token();

        loop {

            if self.current_token_is(TokenType::EOF) || self.current_token_is(TokenType::RBrace) {
                break;
            }

            block.push(self.parse_statement().unwrap());
            self.next_token();
        }
        return Ok(block);
    }

    fn parse_conditional_expr(&mut self) -> Result<Expr, ParserError> {

      let mut alternative = None;

      if !(self.expect_peek(TokenType::LParen)) {
          return Err(ParserError::UnexpectedTokenError(TokenType::LParen, self.peek_token.token_type.clone(), String::from("parse_conditional_expr")));
      } 

      self.next_token();

      let condition = self.parse_expr(Precedence::LOWEST).unwrap();

      if !self.expect_peek(TokenType::RParen) {
        return Err(ParserError::UnexpectedTokenError(TokenType::RParen, self.peek_token.token_type.clone(), String::from("parse_conditional_expr")));
      } 

      if !self.expect_peek(TokenType::LBrace) {
        return Err(ParserError::UnexpectedTokenError(TokenType::LBrace, self.peek_token.token_type.clone(), String::from("parse_conditional_expr")));
      }

//      self.next_token();

      let consequence = self.parse_block_statement().unwrap();

      if self.peek_token_is(TokenType::Else) {

        self.next_token();

        if !self.expect_peek(TokenType::LBrace) {
            return Err(ParserError::UnexpectedTokenError(TokenType::LBrace, self.peek_token.token_type.clone(), String::from("parse_conditional_expr")));
        }

        alternative = Some(self.parse_block_statement().unwrap());

        self.next_token();

      }


    return Ok(Expr::Conditional(Conditional { condition: Box::new(condition), consequence: consequence, alternative: alternative }))

    }

    fn parse_number(&mut self) -> Result<Expr, ParserError> {
      match self.cur_token {
        Token { token_type: _, token_literal: TokenValue::Number(i) } => Ok(Expr::Literal(Literal::Int(i))),
        _ => { Err(ParserError::UnknownTokenError(self.cur_token.token_type.clone())) }
      }
    }

    fn token_precedence(&mut self, token_type: TokenType) -> Precedence {
      match token_type {
          TokenType::Eq | TokenType::NotEq => Precedence::EQUALS,
          TokenType::LessThan | TokenType::GreaterThan => Precedence::LESSGREATER,
          TokenType::Plus | TokenType::Minus => Precedence::SUM,
          TokenType::Asterisk | TokenType::Slash => Precedence::PRODUCT,
          _ => Precedence::LOWEST,
      }
    }

    fn parse_infix_expr(&mut self, left: Expr) -> Result<Expr, ParserError> {
        let infix = match self.cur_token.token_type.clone() {
          TokenType::Plus => Infix::Plus,
          TokenType::Minus => Infix::Minus,
          TokenType::Asterisk => Infix::Multiply,
          TokenType::Eq => Infix::Equal,
          TokenType::NotEq => Infix::NotEqual,
          TokenType::GreaterThan => Infix::GreaterThan,
          TokenType::LessThan => Infix::LessThan,
          TokenType::Slash => Infix::Divide,
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

    fn parse_boolean(&mut self) -> Result<Expr,ParserError> {
        match self.cur_token.token_type {
            TokenType::True => {
                return Ok(Expr::Literal(Literal::Bool(true)))
            }
            TokenType::False => {
               return Ok(Expr::Literal(Literal::Bool(false)))
            },
            _ => Err(ParserError::UnknownTokenError(self.cur_token.token_type.clone()))
        }
    }

    fn parse_grouped_expr(&mut self) -> Result<Expr, ParserError> {

        self.next_token();
        let expr = self.parse_expr(Precedence::LOWEST);

        if !self.expect_peek(TokenType::RParen) {
            return Err(ParserError::UnexpectedTokenError(TokenType::RParen, self.cur_token.token_type.clone(), "parse_grouped_expr".to_string()));
        }

        return expr;

    }

    fn parse_expr(&mut self, precedence: Precedence) -> Result<Expr, ParserError> {

      let mut lhs = match self.cur_token.token_type {
        TokenType::True | TokenType::False => self.parse_boolean(),
        TokenType::LParen => self.parse_grouped_expr(),
        TokenType::Ident => self.parse_ident_expr(),
        TokenType::Number => self.parse_number(),
        TokenType::If => self.parse_conditional_expr(),
        TokenType::Bang 
        | TokenType::Minus | TokenType::Plus  => self.parse_prefix_expr(),
        _ => {
            println!("Error in parse expression!");
            return Err(ParserError::UnknownTokenError(self.cur_token.token_type.clone()));
        }
      }.unwrap();

      while self.peek_token.token_type != TokenType::SemiColon && (precedence < self.token_precedence(self.peek_token.token_type.clone())) {

        match self.peek_token.token_type {
        TokenType::Plus
        | TokenType::Asterisk
        | TokenType::Minus
        | TokenType::Eq
        | TokenType::NotEq | TokenType::Slash
        | TokenType::GreaterThan | TokenType::LessThan => {
            self.next_token();
            lhs = self.parse_infix_expr(lhs).unwrap();
        }
        TokenType::LParen => {
            self.next_token();
        }
        TokenType::RParen => {
            self.next_token();
        }
        _ => return Ok(lhs),
      }

    }

      return Ok(lhs)

    }

    fn parse_return_statement(&mut self) -> Result<Node, ParserError> {

        self.next_token();
        let expr = self.parse_expr(Precedence::LOWEST);

        self.expect_peek(TokenType::SemiColon);

        match expr {
          Ok(expr) => Ok(Node::Statement(Statement::Return(expr))),
          Err(e) => Err(e)
        }

    }

    fn parse_expr_statement(&mut self) -> Result<Node, ParserError> {

      let expr = self.parse_expr(Precedence::LOWEST);

      let res = match expr {
          Ok(expr) => Ok(Node::Statement(Statement::Expr(expr))),
          Err(e) => Err(e)
      };

      if (self.expect_peek(TokenType::SemiColon)) {
          self.next_token();
      }

      return res;

    }

    fn parse_let_statement(&mut self) -> Result<Node, ParserError> {

      if !self.expect_peek(TokenType::Ident) {
        return Err(ParserError::UnexpectedTokenError(TokenType::Ident, self.peek_token.token_type.clone(), String::from("parse_let_statement")));
      }

      let name =  self.cur_token.token_literal.clone();

      if !self.expect_peek(TokenType::Assign) {
        return Err(ParserError::UnexpectedTokenError(TokenType::Assign, self.peek_token.token_type.clone(), String::from("parse_let_statement")));
      }

      self.next_token();

      let expr = match self.parse_expr(Precedence::LOWEST) {
        Ok(expr) => expr,
        Err(e) => return Err(e),
      };

      if !self.expect_peek(TokenType::SemiColon) {
        return Err(ParserError::UnexpectedTokenError(TokenType::SemiColon, self.peek_token.token_type.clone(), String::from("parse_let_statement")));
      }

      Ok(Node::Statement(Statement::Let(Ident(name), expr)))
    }

      fn parse_statement(&mut self) -> Result<Node, ParserError> {

        match self.cur_token.token_type.clone() {
          TokenType::Let => { self.parse_let_statement() },
          TokenType::Return => { self.parse_return_statement() },
          TokenType::Null => { Ok(Node::Statement(Statement::Null)) }
          _ => {
            self.parse_expr_statement()
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
