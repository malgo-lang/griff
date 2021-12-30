use crate::{
    sexpr::{Atom, SExpr},
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
    Symbol(&'static str),
    Atom,
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
                SExpr::Atom(Atom::Symbol(name))
            }
            token if token.is_number() => {
                let number = token.text.clone();
                self.consume(); // consume a number from input
                SExpr::Atom(Atom::Natural(number.parse().unwrap()))
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

            for leading_operator in self.peek_leading_operators(text) {
                // match a leading operator
                self.consume();
                match self.parse_leading_operator(leading_operator) {
                    Some(e) => {
                        expr = Some(e);
                    }
                    None => {
                        self.position = start_position;
                        continue;
                    }
                }
                break;
            }

            match expr {
                Some(expr) => expr,
                // マッチする先行演算子がない場合は、atomをパース
                None => self.parse_atom(),
            }
        };

        'following: loop {
            if let Some(token) = self.peek() {
                let start_position = self.position;
                for following_operator in self.peek_following_operators(&token.text) {
                    // If the precedence of the following operator is greater than the minimum, skip this operator.
                    if following_operator.kind.left_bp() <= min_bp {
                        continue;
                    }

                    self.consume();
                    match self.parse_following_operator(following_operator, leading_expr.clone()) {
                        Some(e) => {
                            leading_expr = e;
                            continue 'following; // continue to the next following operator
                        }
                        None => {
                            self.position = start_position;
                            continue;
                        }
                    }
                }
            }
            return leading_expr;
        }
    }

    // Return all leading operators start from 'text'
    fn peek_leading_operators(&self, text: &str) -> Vec<LeadingOp> {
        let mut operators = vec![];
        for operator in self.language.leading_operators.iter() {
            if let Part::Symbol(name) = operator.parts[0] {
                if name == text {
                    operators.push(operator.clone());
                }
            }
        }
        operators
    }

    fn peek_following_operators(&self, text: &str) -> Vec<FollowingOp> {
        let mut operators = vec![];
        for operator in self.language.following_operators.iter() {
            if let Part::Symbol(name) = operator.parts[0] {
                if name == text {
                    operators.push(operator.clone());
                }
            }
        }
        operators
    }

    fn parse_leading_operator(&mut self, leading_operator: LeadingOp) -> Option<SExpr> {
        let mut children = vec![SExpr::Atom(Atom::Symbol(leading_operator.name.clone()))];

        // 記号の内側部分
        for part in leading_operator.parts[1..].iter() {
            match part {
                Part::Atom => {
                    children.push(self.parse_atom());
                }
                Part::Expr => {
                    children.push(self.parse_expr(0));
                }
                Part::Symbol(name) => match self.peek() {
                    Some(token) if token.text == *name => {
                        self.consume();
                    }
                    _ => {
                        return None;
                    }
                },
            }
        }

        // prefix演算子の場合は、後ろに続く式をパース
        if let LeadingOpKind::Prefix { right_bp } = leading_operator.kind {
            let following_expr = self.parse_expr(right_bp);
            children.push(following_expr);
        }

        Some(SExpr::List(children))
    }

    fn parse_following_operator(
        &mut self,
        following_operator: FollowingOp,
        leading_expr: SExpr,
    ) -> Option<SExpr> {
        let mut children = vec![
            SExpr::Atom(Atom::Symbol(following_operator.name.clone())),
            leading_expr,
        ];

        // 記号の内側部分
        for part in following_operator.parts[1..].iter() {
            match part {
                Part::Atom => {
                    children.push(self.parse_atom());
                }
                Part::Expr => {
                    children.push(self.parse_expr(0));
                }
                Part::Symbol(name) => match self.peek() {
                    Some(token) if token.text == *name => {
                        self.consume();
                    }
                    _ => {
                        return None;
                    }
                },
            }
        }

        // If the following operator is a infix operator, parse the following expression.
        if let FollowingOpKind::Infix { right_bp, .. } = following_operator.kind {
            let following_expr = self.parse_expr(right_bp);
            children.push(following_expr);
        }

        Some(SExpr::List(children))
    }
}

pub fn complete_parse(input: &str, expected: &str) {
    use Part::*;
    let language = Language::new(
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
                    Expr,
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
    );

    let tokens = Tokenizer::new(input, 0).tokenize();

    let mut parser = Parser::new(tokens, 0, language);
    let expr = parser.parse_expr(0);
    assert_eq!(expr.to_string(), expected);
}

#[test]
/** simple arithmetic expression */
fn test_simple_arithmetic_expression() {
    // 1 -> 1
    complete_parse("1", "1");
    // -1 -> (- 1)
    complete_parse("-1", "(- 1)");
    // 1 + 2 -> (+ 1 2)
    complete_parse("1 + 2", "(+ 1 2)");
    // 1 + 2 * 3 -> (+ 1 (* 2 3))
    complete_parse("1 + 2 * 3", "(+ 1 (* 2 3))");
    // 1 + 2 * 3 - 4 -> (- (+ 1 (* 2 3)) 4)
    complete_parse("1 + 2 * 3 - 4", "(- (+ 1 (* 2 3)) 4)");
    // 1 + -2 -> (+ 1 (- 2))
    complete_parse("1 + -2", "(+ 1 (- 2))");
    // 1 - -2 -> (- 1 (- 2))
    complete_parse("1 - -2", "(- 1 (- 2))");
}

#[test]
/** simple expression included other operators, e.g. paren, call, call-block, ... */
fn test_simple_other_expression() {
    // 1 = 1 -> (= 1 1)
    complete_parse("1 = 1", "(= 1 1)");
    // 1 = 1 + 2 -> (= 1 (+ 1 2))
    complete_parse("1 = 1 + 2", "(= 1 (+ 1 2))");
    // 2 * 3 = 1 + 2 -> (= (* 2 3) (+ 1 2))
    complete_parse("2 * 3 = 1 + 2", "(= (* 2 3) (+ 1 2))");
    // 1 = 1 = 1 -> (= (= 1 1) 1)
    complete_parse("1 = 1 = 1", "(= (= 1 1) 1)");
    // (- 1) -> (paren (- 1))
    complete_parse("(- 1)", "(paren (- 1))");
    // 1 ? -> (? 1)
    complete_parse("1 ?", "(? 1)");
    // lambda x.x -> (lambda x x)
    complete_parse("lambda x.x", "(lambda x x)");
    // (lambda x.x)(1) -> (call (paren (lambda x x)) 1)
    complete_parse("(lambda x.x)(1)", "(call (paren (lambda x x)) 1)");
    // fn(x){x}(1) -> (call (fn x x) 1)
    complete_parse("fn(x){x}(1)", "(call (fn x x) 1)");
    // map(list){ it * 2 } -> (call-block map list (* it 2))
    complete_parse("map(list){ it * 2 }", "(call-block map list (* it 2))");
    // array[index] -> (subscript array index)
    complete_parse("array[index]", "(subscript array index)");
}

#[test]
// Test parsing of expressions with `,`
fn test_comma_list() {
    // 1, 2 -> (, 1 2)
    complete_parse("1, 2", "(, 1 2)");
    // 1, 2, 3 -> (, 1 (, 2 3))
    complete_parse("1, 2, 3", "(, 1 (, 2 3))");

    // fn(x, y){x + y} -> (fn (, x y) (+ x y))
    complete_parse("fn(x, y){x + y}", "(fn (, x y) (+ x y))");
    // fn(x, y, z){x + y + z} -> (fn (, x (, y z)) (+ (+ x y) z))
    complete_parse("fn(x, y, z){x + y + z}", "(fn (, x (, y z)) (+ (+ x y) z))");
}
