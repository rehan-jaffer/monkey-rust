mod lexer;
mod token;
mod repl;

const input_code : &str = "
let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result == add(five, ten);
";

fn main() {

   let repl = repl::REPL { };
   repl.start();

}
