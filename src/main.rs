mod ast;
mod parser;

use std::io::{self, Read};

fn main() -> io::Result<()> {
    // Read from stdin
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    println!("{:?}", parser::parse(&input));
    Ok(())
}
