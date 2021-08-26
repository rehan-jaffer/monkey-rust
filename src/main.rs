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
return 1;
";

// let result = add(five, ten);
// let add = fn(x, y) {
// x + y;
// };

fn main() {

   let mut repl = repl::REPL::new();
   repl.start();

}