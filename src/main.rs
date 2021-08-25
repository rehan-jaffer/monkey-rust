#[macro_use]
extern crate lazy_static;

mod lexer;
mod token;
mod repl;
mod parser;

use std::cell::RefCell;


use std::str::Chars;
use std::iter::Peekable;

const input_code : &str = "
let five = 5;
let ten = 10;
let six = -10 + 5;
";

// let result = add(five, ten);
// let add = fn(x, y) {
// x + y;
// };

fn main() {

//   let repl = repl::REPL { };
//   repl.start();

   println!("Parsing program: {}", input_code);
   let mut lexer = lexer::Lexer { input: input_code.chars().peekable() };
   let mut parser = parser::Parser::new(&mut lexer);
   let program = parser.parse_program();

   let mut lexer = lexer::Lexer { input: input_code.chars().peekable() };

   println!("Tokens: ");
   loop {
      let tok = lexer.nextToken();
      println!("{:?}", tok);
      if tok.token_type == token::TokenType::EOF {
         break;
      }
   } 

   println!("\r\nAST: ");
   for statement in program.statements {
      println!("Node -> {:#?}", statement);
   }

}
