#[cfg(test)]

mod test {

  use monkey; 

  const TEST_CODE : &str = "let x = 1 + 2 - 1;";

  #[test]
  fn it_works() {
    let mut lexer = monkey::lexer::Lexer { input: TEST_CODE.chars().peekable() };
    let mut parser = monkey::parser::Parser::new(&mut lexer);
    let res = parser.parse_program().statements.pop().unwrap();

    assert_eq!(res.serialize(), "(let x (- (+ 1 2) 1))");
  }

}
