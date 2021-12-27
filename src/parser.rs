use crate::{
    sexpr::SExpr,
    token::{Token, Tokenizer},
};

// https://zenn.dev/pandaman64/books/pratt-parsing

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LeadingOpKind {
    Prefix { right_bp: u16 },
    Paren,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FollowingOpKind {
    Postfix { left_bp: u16 },
    Infix { left_bp: u16, right_bp: u16 },
}

impl FollowingOpKind {
    fn left_bp(&self) -> u16 {
        match self {
            FollowingOpKind::Infix { left_bp, .. } => *left_bp,
            FollowingOpKind::Postfix { left_bp } => *left_bp,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Part {
    Symbol { name: String },
    Expr,
}

#[derive(Debug, Clone)]
pub struct Operator<K> {
    kind: K,
    name: String,
    parts: Vec<Part>,
}

pub type LeadingOp = Operator<LeadingOpKind>;
pub type FollowingOp = Operator<FollowingOpKind>;

pub fn prefix(name: String, parts: Vec<Part>, right_bp: u16) -> LeadingOp {
    LeadingOp {
        kind: LeadingOpKind::Prefix { right_bp },
        name,
        parts,
    }
}

pub fn paren(name: String, parts: Vec<Part>) -> LeadingOp {
    LeadingOp {
        kind: LeadingOpKind::Paren,
        name,
        parts,
    }
}

pub fn postfix(name: String, parts: Vec<Part>, left_bp: u16) -> FollowingOp {
    FollowingOp {
        kind: FollowingOpKind::Postfix { left_bp },
        name,
        parts,
    }
}

pub fn infix(name: String, parts: Vec<Part>, left_bp: u16, right_bp: u16) -> FollowingOp {
    FollowingOp {
        kind: FollowingOpKind::Infix { left_bp, right_bp },
        name,
        parts,
    }
}

#[derive(Debug)]
pub struct Language {
    leading_operators: Vec<LeadingOp>,
    following_operators: Vec<FollowingOp>,
}

impl Language {
    pub fn new(leading_operators: Vec<LeadingOp>, following_operators: Vec<FollowingOp>) -> Self {
        Self {
            leading_operators,
            following_operators,
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
    language: Language,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, position: usize, language: Language) -> Self {
        Self {
            tokens,
            position,
            language,
        }
    }

    // read one character
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn consume(&mut self) {
        self.position += 1;
    }

    // Parse a S-expression
    fn parse_atom(&mut self) -> SExpr {
        match self.peek().unwrap() {
            token if token.is_symbol() => {
                let name = token.text.clone();
                self.consume(); // consume a symbol from input
                SExpr::Atom(name)
            }
            token if token.is_number() => {
                let number = token.text.clone();
                self.consume(); // consume a number from input
                SExpr::Atom(number)
            }
            token => panic!("expected an atom, got {:?}", token),
        }
    }

    pub fn parse_expr(&mut self, min_bp: u16) -> SExpr {
        let mut leading_expr = {
            let mut expr = None;
            let token = self.peek().unwrap();
            let text = &token.text;

            let start_position = self.position;

            'leading: for leading_operator in self.peek_leading_operators(text) {
                // match a leading operator
                self.consume();
                let mut children = vec![SExpr::Atom(leading_operator.name.clone())];

                // 記号の内側部分
                for part in leading_operator.parts[1..].iter() {
                    match part {
                        Part::Expr => {
                            children.push(self.parse_expr(0));
                        }
                        Part::Symbol { name } => match self.peek() {
                            Some(token) if token.text == *name => {
                                self.consume();
                            }
                            _ => {
                                self.position = start_position;
                                continue 'leading;
                            }
                        },
                    }
                }

                // prefix演算子の場合は、後ろに続く式をパース
                if let LeadingOpKind::Prefix { right_bp } = leading_operator.kind {
                    let following_expr = self.parse_expr(right_bp);
                    children.push(following_expr);
                }

                expr = Some(SExpr::List(children));
                break;
            }

            match expr {
                Some(expr) => expr,
                // マッチする先行演算子がない場合は、atomをパース
                None => self.parse_atom(),
            }
        };

        'outer: loop {
            if let Some(token) = self.peek() {
                let start_position = self.position;
                'inner: for following_operator in self.peek_following_operators(&token.text) {
                    // If the precedence of the following operator is greater than the minimum, skip this operator.
                    if following_operator.kind.left_bp() <= min_bp {
                        continue 'inner;
                    }

                    self.consume();
                    let mut children = vec![
                        SExpr::Atom(following_operator.name.clone()),
                        leading_expr.clone(),
                    ];

                    // 記号の内側部分
                    for part in following_operator.parts[1..].iter() {
                        match part {
                            Part::Expr => {
                                children.push(self.parse_expr(0));
                            }
                            Part::Symbol { name } => {
                                match self.peek() {
                                    Some(token) if token.text == *name => {
                                        self.consume();
                                    }
                                    _ => {
                                        // backtracking
                                        self.position = start_position;
                                        continue 'inner;
                                    }
                                }
                            }
                        }
                    }

                    // If the following operator is a infix operator, parse the following expression.
                    if let FollowingOpKind::Infix { right_bp, .. } = following_operator.kind {
                        let following_expr = self.parse_expr(right_bp);
                        children.push(following_expr);
                    }

                    leading_expr = SExpr::List(children);
                    continue 'outer; // continue to the next following operator
                }
            }
            break;
        }
        return leading_expr;
    }

    // Return all leading operators start from 'text'
    fn peek_leading_operators(&self, text: &String) -> Vec<LeadingOp> {
        let mut operators = vec![];
        for operator in self.language.leading_operators.iter() {
            match &operator.parts[0] {
                Part::Symbol { name } => {
                    if *name == *text {
                        operators.push(operator.clone());
                    }
                }
                _ => {}
            }
        }
        return operators;
    }

    fn peek_following_operators(&self, text: &String) -> Vec<FollowingOp> {
        let mut operators = vec![];
        for operator in self.language.following_operators.iter() {
            match &operator.parts[0] {
                Part::Symbol { name } => {
                    if *name == *text {
                        operators.push(operator.clone());
                    }
                }
                _ => {}
            }
        }
        return operators;
    }
}

pub fn complete_parse(input: &str, expected: &str) {
    use Part::*;
    let language = Language::new(
        vec![
            prefix(
                "-".into(),
                vec![Symbol {
                    name: "-".to_string(),
                }],
                51,
            ),
            prefix(
                "if-then-else".into(),
                vec![
                    Symbol {
                        name: "if".to_string(),
                    },
                    Expr,
                    Symbol {
                        name: "then".to_string(),
                    },
                    Expr,
                    Symbol {
                        name: "else".to_string(),
                    },
                ],
                41,
            ),
            prefix(
                "lambda".into(),
                vec![
                    Symbol {
                        name: "lambda".to_string(),
                    },
                    Expr,
                    Symbol {
                        name: ".".to_string(),
                    },
                ],
                0,
            ),
            paren(
                "fn".into(),
                vec![
                    Symbol {
                        name: "fn".to_string(),
                    },
                    Symbol {
                        name: "(".to_string(),
                    },
                    Expr,
                    Symbol {
                        name: ")".to_string(),
                    },
                    Symbol {
                        name: "{".to_string(),
                    },
                    Expr,
                    Symbol {
                        name: "}".to_string(),
                    },
                ],
            ),
            paren(
                "paren".into(),
                vec![
                    Symbol {
                        name: "(".to_string(),
                    },
                    Expr,
                    Symbol {
                        name: ")".to_string(),
                    },
                ],
            ),
        ],
        vec![
            postfix(
                "?".into(),
                vec![Symbol {
                    name: "?".to_string(),
                }],
                20,
            ),
            postfix(
                "subscript".into(),
                vec![
                    Symbol {
                        name: "[".to_string(),
                    },
                    Expr,
                    Symbol {
                        name: "]".to_string(),
                    },
                ],
                100,
            ),
            // バックトラックが必要？
            postfix(
                "call-block".into(),
                vec![
                    Symbol {
                        name: "(".to_string(),
                    },
                    Expr,
                    Symbol {
                        name: ")".to_string(),
                    },
                    Symbol {
                        name: "{".to_string(),
                    },
                    Expr,
                    Symbol {
                        name: "}".to_string(),
                    },
                ],
                100,
            ),
            postfix(
                "call".into(),
                vec![
                    Symbol {
                        name: "(".to_string(),
                    },
                    Expr,
                    Symbol {
                        name: ")".to_string(),
                    },
                ],
                100,
            ),
            infix(
                "+".into(),
                vec![Symbol {
                    name: "+".to_string(),
                }],
                50,
                51,
            ),
            infix(
                "-".into(),
                vec![Symbol {
                    name: "-".to_string(),
                }],
                50,
                51,
            ),
            infix(
                "*".into(),
                vec![Symbol {
                    name: "*".to_string(),
                }],
                80,
                81,
            ),
            infix(
                "=".into(),
                vec![Symbol {
                    name: "=".to_string(),
                }],
                21,
                20,
            ),
        ],
    );

    let tokens = Tokenizer::new(input, 0).tokenize();

    let mut parser = Parser::new(tokens, 0, language);
    let expr = parser.parse_expr(0);
    assert_eq!(expr.to_string(), expected);
}

#[test]
fn test_parse_atom() {
    complete_parse("7", "7");
}

#[test]
fn test_simple_prefix() {
    complete_parse("-7", "(- 7)");
}

#[test]
fn test_paren() {
    complete_parse("(-7)", "(paren (- 7))");
}

#[test]
fn test_simple_postfix() {
    complete_parse("7?", "(? 7)");
}

#[test]
fn test_simple_infix() {
    complete_parse("7+8", "(+ 7 8)");
}

#[test]
fn test_infix_and_prefix() {
    complete_parse("7 + -8", "(+ 7 (- 8))");
}

#[test]
fn test_different_position() {
    complete_parse("1 - -2", "(- 1 (- 2))")
}

#[test]
fn test_complex() {
    complete_parse(
        "1 = 2 = if 3 then(4) else( 5[6] )",
        "(= 1 (= 2 (if-then-else 3 (paren 4) (paren (subscript 5 6)))))",
    )
}

#[test]
fn test_lambda() {
    complete_parse("lambda x.x", "(lambda x x)")
}

#[test]
fn test_call() {
    complete_parse("(lambda x.x)(7)", "(call (paren (lambda x x)) 7)")
}

#[test]
fn test_fn() {
    complete_parse("fn(x){x}", "(fn x x)")
}

#[test]
fn test_complex_fn() {
    complete_parse("fn (x) { x - 7 }", "(fn x (- x 7))")
}

#[test]
fn test_call_fn() {
    complete_parse("fn(x){x}(7)", "(call (fn x x) 7)")
}

#[test]
fn test_call_block_fn() {
    complete_parse(
        "(fn(x){x})(y){7}",
        "(call-block (paren (fn x x)) y 7)",
    )
}
