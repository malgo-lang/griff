mod sexpr;
mod parser;
mod token;

use parser::complete_parse;

fn main() {
    complete_parse("1+2", "(+ 1 2)")
}
