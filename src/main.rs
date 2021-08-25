#[macro_use]
extern crate lazy_static;

mod lexer;
mod token;
mod repl;
mod parser;

use std::cell::RefCell;


const INPUT_CODE : &str = "
let five = 5;
let ten = 10;
let x = -9 + 1;
let y = 1 + 2 + 3 + 4 +5;
let z = 1 - 2 + 3 + 4;
let i = 2 - 1 + -10;
";

// let result = add(five, ten);
// let add = fn(x, y) {
// x + y;
// };

fn main() {

//   let repl = repl::REPL { };
//   repl.start();

   println!("Parsing program: {}", INPUT_CODE);
   let mut lexer = lexer::Lexer { input: INPUT_CODE.chars().peekable() };
   let mut parser = parser::Parser::new(&mut lexer);
   let program = parser.parse_program();

   let mut lexer = lexer::Lexer { input: INPUT_CODE.chars().peekable() };

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
     statement.serialize();
     println!("");
   }

}