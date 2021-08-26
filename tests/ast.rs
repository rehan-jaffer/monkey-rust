#[cfg(test)]

mod test {

  use monkey; 

  fn parse_statement(code: &str) -> String {
    let mut lexer = monkey::lexer::Lexer { input: code.chars().peekable() };
    let mut parser = monkey::parser::Parser::new(&mut lexer);
    let res = parser.parse_program().statements.pop().unwrap(); 
    return res.serialize();
  }

  #[test]
  fn it_parses_simple_let_statement() {
    assert_eq!(parse_statement("let x = 1;"), "(let x 1)");
  }

  #[test]
  fn it_parses_let_statement_with_addition() {
    assert_eq!(parse_statement("let x = 1 + 1;"), "(let x (+ 1 1))");
  }

  #[test]
  fn it_parses_nested_infix_operators() {
    assert_eq!(parse_statement("let z = 1 - 1 + 1;"), "(let z (+ (- 1 1) 1))");
  }

  #[test]
  fn it_parses_prefix_operators() {
    assert_eq!(parse_statement("let z = -1;"), "(let z (- 1))");
  }

  #[test]
  fn it_parses_nested_prefix_operators() {
    assert_eq!(parse_statement("let z = !-1;"), "(let z (! (- 1)))");
  }

  #[test]
  fn it_parses_nested_prefix_and_infix_operators() {
    assert_eq!(parse_statement("let z = !-1 + !-1;"), "(let z (+ (! (- 1)) (! (- 1))))");
  }

}
