mod sexpr;
mod parser;
mod token;
mod eval;

use parser::complete_parse;

fn main() {
    complete_parse("1+2", "(+ 1 2)")
}
