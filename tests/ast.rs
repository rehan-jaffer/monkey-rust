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

  #[test]
  fn it_parses_boolean_literals() {
    assert_eq!(parse_statement("let z = false;"), "(let z false)");
  }

  #[test]
  fn it_parses_return_statements() {
    assert_eq!(parse_statement("return 1 + 1;"), "(ret (+ 1 1))");
  }

  #[test]
  fn it_parses_complicated_return_statements() {
    assert_eq!(parse_statement("return 1 + 1 - !3;"), "(ret (- (+ 1 1) (! 3)))")
  }

  #[test]
  fn it_allows_grouped_expressions_to_change_precedence() {
    assert_eq!(parse_statement("return 1 + (1 * 3);"), "(ret (+ 1 (* 1 3)))");
    assert_eq!(parse_statement("return (1 + 1) * 3;"), "(ret (* (+ 1 1) 3))");
  }

  #[test]
  fn it_parses_all_prefix_operators() {
    assert_eq!(parse_statement("let x = 1 + 2 - 3 / 4 - 5 == 6 == 7 < 8 > 9;"), "(let x (== (== (- (- (+ 1 2) (/ 3 4)) 5) 6) (> (< 7 8) 9)))");
  }

  #[test]
  fn it_parses_if_statements() {
    assert_eq!(parse_statement("if (1 > 2) { return 1; };"), "(( (> 1 2)) ? ((ret 1)) : ())");
  }
  
  #[test]
  fn it_parses_else_statements() {
    assert_eq!(parse_statement("if (1 > 2) { return 1; } else { return 2; };"), "(( (> 1 2)) ? ((ret 1)) : ((ret 2)))");
  }
}
