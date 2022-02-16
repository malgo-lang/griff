mod pratt;
mod sexpr;

use pratt::parser::Parser;
use pratt::token::Tokenizer;

fn main() {
    // Read from stdin
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    // Split by newline
    let parts = input.split("\n");
    // Parse all parts
    for part in parts {
        let language = pratt::parser_test::test_language();
        let tokens = Tokenizer::new(part, 0).tokenize();
        let mut parser = Parser::new(tokens, 0, language);
        let result = parser.parse_expr(0);
        println!("{:?}", result);
    }
}
