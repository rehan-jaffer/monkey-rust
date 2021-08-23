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
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);
";

fn main() {

//   let repl = repl::REPL { };
//   repl.start();

   let mut lexer = lexer::Lexer { input: input_code.chars().peekable() };
   let mut parser = parser::Parser::new(&mut lexer);
   let program = parser.parse_program();

   for statement in program.statements {
      println!("{:?}", statement);
   }

}
