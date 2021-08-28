use std::{io, io::prelude::*};
use std::str::Chars;
use std::iter::Peekable;
use std::fs::File;
use std::io::{BufRead};
use std::path::Path;
use super::parser::{Statement, Node};
use super::token::{Token, TokenType};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use ansi_term::Style;

const PROMPT : &str = ">>";

#[derive(Debug, Clone)]
enum Mode {
    Parse,
    Tokenize,
    ParseToSexp
}

pub struct REPL {
  mode: Mode,
  history: History
}

struct History {
    lines: Vec<String>
}

impl History {
    pub fn add(&mut self, command: String) {
      self.lines.push(command);
    }
}

impl REPL {

    pub fn new() -> Self {
        return Self {
            mode: Mode::Tokenize,
            history: History { lines: vec![] }
        }
    }

    fn motd(&mut self) {
        if let Ok(lines) = read_lines("./motd.txt") {
            // Consumes the iterator, returns an (Optional) String
            for line in lines {
                if let Ok(ip) = line {
                    println!("{}", ip);
                }
            }
        }
    }

    pub fn start(&mut self) {
    
    self.motd();
    self.read_loop();


  }

  fn prompt(&self) -> String {
    let prompt =  Style::new().bold().paint(format!("|saki ({:?}) $> ", self.mode)).to_string();
    return prompt;
}

fn tokenize_input(&self, input: String) -> Vec<Token> {
    let mut lexer = super::lexer::Lexer { input: input.chars().peekable() };
    let mut tokens = Vec::new();

    loop {

        let tok = lexer.nextToken();
        tokens.push(tok.clone());

        if tok.token_type == TokenType::EOF {
            break;
        }

    }

    return tokens;
}

fn parse_input(&self, input: String) -> Node {
    let mut lexer = super::lexer::Lexer { input: input.chars().peekable() };
    let mut parser = super::parser::Parser::new(&mut lexer);
    let mut program = parser.parse_program();
    return program.statements.pop().unwrap();
}

fn repl_notify(&mut self, notification: &str) {
    REPL::print_bold(format!("** NOTICE {}", notification).as_str());
}

fn set_mode(&mut self, mode: Mode) {

    self.mode = mode.clone();
    self.repl_notify(format!("Changed mode to {:?}", mode).as_str());
    return ();

}

fn print_bold(text: &str) {
  println!("{}", Style::new().bold().paint(text));
}

  fn read_loop(&mut self) {

    let mut rl = Editor::<()>::new();

    let stdin = io::stdin();

    loop {

        let input = rl.readline(self.prompt().as_str());

        match input {
            Ok(line) => {

                match line.as_str() {
                    "exit" => { 
                        println!("Bye! Have a very monkey day!");
                        return () 
                    },
                    ":p" => { self.set_mode(Mode::Parse) },
                    ":pex" => self.set_mode(Mode::ParseToSexp),
                    ":t" => self.set_mode(Mode::Tokenize),
                    _ => {
                        rl.add_history_entry(line.as_str());
                        match self.mode {
                            Mode::Parse => {
                              let res = self.parse_input(line);
                              println!("s > {:?}", res);    
                            },
                            Mode::ParseToSexp => {
                              let res = self.parse_input(line);
                              println!("s > {}", res.serialize());    
                            }
                            Mode::Tokenize => {
                              let res = self.tokenize_input(line);
                              for token in res {
                                println!("<Token:{:?}:{:?}>", token.token_type, token.token_literal);
                              }
                            }
                          }          
                    }
                }
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
    

        }

        
    }

  }
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}