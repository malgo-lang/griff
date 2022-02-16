use crate::pratt::parser::*;
#[cfg(test)]
use crate::pratt::token::Tokenizer;

pub fn test_language() -> Language {
    use Part::*;
    Language::new(
        vec![
            // prefix - ... 51
            prefix("-".into(), vec![Symbol("-")], 51),
            // prefix if <expr> then <expr> else ... 41
            prefix(
                "if-then-else".into(),
                vec![Symbol("if"), Expr, Symbol("then"), Expr, Symbol("else")],
                41,
            ),
            // prefix lambda <expr> . ... 0
            prefix(
                "lambda".into(),
                vec![Symbol("lambda"), Atom, Symbol(".")],
                0,
            ),
            // paren fn ( <expr> ) { <expr> }
            paren(
                "fn".into(),
                vec![
                    Symbol("fn"),
                    Symbol("("),
                    List(Box::new(Expr)),
                    Symbol(")"),
                    Symbol("{"),
                    Expr,
                    Symbol("}"),
                ],
            ),
            // paren ( <expr> )
            paren("paren".into(), vec![Symbol("("), Expr, Symbol(")")]),
        ],
        vec![
            // postfix ... ? 20
            postfix("?".into(), vec![Symbol("?")], 20),
            // postfix ... [ <expr> ] 100
            postfix(
                "subscript".into(),
                vec![Symbol("["), Expr, Symbol("]")],
                100,
            ),
            // バックトラックが必要
            // postfix ... ( <expr> ) { <expr> } 100
            postfix(
                "call-block".into(),
                vec![
                    Symbol("("),
                    Expr,
                    Symbol(")"),
                    Symbol("{"),
                    Expr,
                    Symbol("}"),
                ],
                100,
            ),
            // postfix ... ( <expr> ) 100
            postfix("call".into(), vec![Symbol("("), Expr, Symbol(")")], 100),
            // infix ... + ... 50 51
            infix("+".into(), vec![Symbol("+")], 50, 51),
            // infix ... - ... 50 51
            infix("-".into(), vec![Symbol("-")], 50, 51),
            // infix ... * ... 80 81
            infix("*".into(), vec![Symbol("*")], 80, 81),
            // infix ... = ... 21 20
            infix("=".into(), vec![Symbol("=")], 20, 20),
            // infix ... , ... 1 2
            // 0, 1 don't work.
            infix(",".into(), vec![Symbol(",")], 2, 1),
        ],
    )
}

#[cfg(test)]
pub fn complete_parse(language: Language, input: &str, expected: &str) {
    let tokens = Tokenizer::new(input, 0).tokenize();

    let mut parser = Parser::new(tokens, 0, language);
    let expr = parser.parse_expr(0);
    match expr {
        Ok(expr) => assert_eq!(expr.to_string(), expected),
        Err(err) => panic!("Parse error: {:?}", err),
    }
}

#[test]
/** simple arithmetic expression */
fn test_simple_arithmetic_expression() {
    // 1 -> 1
    complete_parse(test_language(), "1", "1");
    // -1 -> (- 1)
    complete_parse(test_language(), "-1", "(- 1)");
    // 1 + 2 -> (+ 1 2)
    complete_parse(test_language(), "1 + 2", "(+ 1 2)");
    // 1 + 2 * 3 -> (+ 1 (* 2 3))
    complete_parse(test_language(), "1 + 2 * 3", "(+ 1 (* 2 3))");
    // 1 + 2 * 3 - 4 -> (- (+ 1 (* 2 3)) 4)
    complete_parse(test_language(), "1 + 2 * 3 - 4", "(- (+ 1 (* 2 3)) 4)");
    // 1 + -2 -> (+ 1 (- 2))
    complete_parse(test_language(), "1 + -2", "(+ 1 (- 2))");
    // 1 - -2 -> (- 1 (- 2))
    complete_parse(test_language(), "1 - -2", "(- 1 (- 2))");
}

#[test]
/** simple expression included other operators, e.g. paren, call, call-block, ... */
fn test_simple_other_expression() {
    // 1 = 1 -> (= 1 1)
    complete_parse(test_language(), "1 = 1", "(= 1 1)");
    // 1 = 1 + 2 -> (= 1 (+ 1 2))
    complete_parse(test_language(), "1 = 1 + 2", "(= 1 (+ 1 2))");
    // 2 * 3 = 1 + 2 -> (= (* 2 3) (+ 1 2))
    complete_parse(test_language(), "2 * 3 = 1 + 2", "(= (* 2 3) (+ 1 2))");
    // 1 = 1 = 1 -> (= (= 1 1) 1)
    complete_parse(test_language(), "1 = 1 = 1", "(= (= 1 1) 1)");
    // (- 1) -> (paren (- 1))
    complete_parse(test_language(), "(- 1)", "(paren (- 1))");
    // 1 ? -> (? 1)
    complete_parse(test_language(), "1 ?", "(? 1)");
    // lambda x.x -> (lambda x x)
    complete_parse(test_language(), "lambda x.x", "(lambda x x)");
    // (lambda x.x)(1) -> (call (paren (lambda x x)) 1)
    complete_parse(
        test_language(),
        "(lambda x.x)(1)",
        "(call (paren (lambda x x)) 1)",
    );
    // fn(x){x}(1) -> (call (fn (x) x) 1)
    complete_parse(test_language(), "fn(x){x}(1)", "(call (fn (x) x) 1)");
    // map(list){ it * 2 } -> (call-block map list (* it 2))
    complete_parse(
        test_language(),
        "map(list){ it * 2 }",
        "(call-block map list (* it 2))",
    );
    // array[index] -> (subscript array index)
    complete_parse(test_language(), "array[index]", "(subscript array index)");
}

#[test]
// Test parsing of expressions with `,`
fn test_comma_list() {
    // 1, 2 -> (, 1 2)
    complete_parse(test_language(), "1, 2", "(, 1 2)");
    // 1, 2, 3 -> (, 1 (, 2 3))
    complete_parse(test_language(), "1, 2, 3", "(, 1 (, 2 3))");
    // fn(x, y){x + y} -> (fn ((, x y)) (+ x y))
    complete_parse(test_language(), "fn(x, y){x + y}", "(fn ((, x y)) (+ x y))");
    // fn(x, y, z){x + y + z} -> (fn ((, x (, y z))) (+ (+ x y) z))
    complete_parse(
        test_language(),
        "fn(x, y, z){x + y + z}",
        "(fn ((, x (, y z))) (+ (+ x y) z))",
    );
}
