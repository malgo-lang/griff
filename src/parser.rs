use crate::{
    sexpr::SExpr,
    token::{Token, Tokenizer},
};

// https://zenn.dev/pandaman64/books/pratt-parsing

#[derive(Debug)]
pub struct Input {
    tokens: Vec<Token>,
    position: usize,
}

impl Input {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    // read one character
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    pub fn bump(&mut self) {
        self.position += 1;
    }
}

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
pub struct Operator<K> {
    kind: K,
    name: String,
    symbols: Vec<String>,
}

pub type LeadingOp = Operator<LeadingOpKind>;
pub type FollowingOp = Operator<FollowingOpKind>;

pub fn prefix(name: String, symbols: Vec<String>, right_bp: u16) -> LeadingOp {
    LeadingOp {
        kind: LeadingOpKind::Prefix { right_bp },
        name,
        symbols,
    }
}

pub fn paren(name: String, symbols: Vec<String>) -> LeadingOp {
    LeadingOp {
        kind: LeadingOpKind::Paren,
        name,
        symbols,
    }
}

pub fn postfix(name: String, symbols: Vec<String>, left_bp: u16) -> FollowingOp {
    FollowingOp {
        kind: FollowingOpKind::Postfix { left_bp },
        name,
        symbols,
    }
}

pub fn infix(name: String, symbols: Vec<String>, left_bp: u16, right_bp: u16) -> FollowingOp {
    FollowingOp {
        kind: FollowingOpKind::Infix { left_bp, right_bp },
        name,
        symbols,
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

pub struct Parser<'a> {
    input: &'a mut Input,
    language: Language,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a mut Input, language: Language) -> Self {
        Self { input, language }
    }

    fn parse_atom(&mut self) -> SExpr {
        match self.input.peek().unwrap() {
            token if token.is_symbol() => {
                let name = token.text.clone();
                self.input.bump(); // consume a symbol from input
                SExpr::Atom(name)
            }
            token if token.is_number() => {
                let number = token.text.clone();
                self.input.bump(); // consume a number from input
                SExpr::Atom(number)
            }
            token => panic!("expected an atom, got {:?}", token),
        }
    }

    pub fn parse_expr(&mut self, min_bp: u16) -> SExpr {
        let mut leading_expr = {
            let mut expr = None;
            let token = self.input.peek().unwrap();
            let text = &token.text;

            if let Some(leading_operator) = self.peek_leading_operator(text) {
                // match a leading operator
                self.input.bump();
                let mut children = vec![SExpr::Atom(leading_operator.name.clone())];

                // 記号の内側部分
                for symbol in leading_operator.symbols[1..].iter() {
                    let inner_expr = self.parse_expr(0);
                    children.push(inner_expr);

                    assert_eq!(self.input.peek().unwrap().text, *symbol);
                    self.input.bump();
                }

                // prefix演算子の場合は、後ろに続く式をパース
                if let LeadingOpKind::Prefix { right_bp } = leading_operator.kind {
                    let following_expr = self.parse_expr(right_bp);
                    children.push(following_expr);
                }

                expr = Some(SExpr::List(children));
            }

            match expr {
                Some(expr) => expr,
                // マッチする先行演算子がない場合は、atomをパース
                None => self.parse_atom(),
            }
        };

        loop {
            if let Some(token) = self.input.peek() {
                if let Some(following_operator) = self.peek_following_operator(&token.text) {
                    // 演算子の優先順位が足りない場合はやめる
                    if following_operator.kind.left_bp() <= min_bp {
                        return leading_expr;
                    }

                    self.input.bump();
                    let mut children =
                        vec![SExpr::Atom(following_operator.name.clone()), leading_expr];

                    // 記号の内側部分
                    for symbol in following_operator.symbols[1..].iter() {
                        let inner_expr = self.parse_expr(0);
                        children.push(inner_expr);

                        assert_eq!(self.input.peek().unwrap().text, *symbol);
                        self.input.bump();
                    }

                    // infix演算子の場合は後ろに続く式をパース
                    if let FollowingOpKind::Infix { right_bp, .. } = following_operator.kind {
                        let following_expr = self.parse_expr(right_bp);
                        children.push(following_expr);
                    }

                    leading_expr = SExpr::List(children);
                    continue; // 残りの後続演算子を再帰的にパース
                }
            }
            break;
        }
        return leading_expr;
    }

    fn peek_leading_operator(&self, text: &String) -> Option<LeadingOp> {
        for operator in self.language.leading_operators.iter() {
            if operator.symbols[0] == *text {
                return Some(operator.clone());
            }
        }
        return None;
    }

    fn peek_following_operator(&self, text: &String) -> Option<FollowingOp> {
        for operator in self.language.following_operators.iter() {
            if operator.symbols[0] == *text {
                return Some(operator.clone());
            }
        }
        return None;
    }
}

pub fn complete_parse(input: &str, expected: &str) {
    let language = Language::new(
        vec![
            prefix("-".into(), vec!["-".to_string()], 51),
            prefix(
                "if-then-else".into(),
                vec!["if".to_string(), "then".to_string(), "else".to_string()],
                41,
            ),
            prefix(
                "lambda".into(),
                vec!["lambda".to_string(), ".".to_string()],
                0,
            ),
            paren("paren".into(), vec!["(".to_string(), ")".to_string()]),
        ],
        vec![
            postfix("?".into(), vec!["?".to_string()], 20),
            postfix(
                "subscript".into(),
                vec!["[".to_string(), "]".to_string()],
                100,
            ),
            postfix(
                "call".into(),
                vec!["(".to_string(), ")".to_string()],
                100,
            ),
            infix("+".into(), vec!["+".to_string()], 50, 51),
            infix("-".into(), vec!["-".to_string()], 50, 51),
            infix("*".into(), vec!["*".to_string()], 80, 81),
            infix("=".into(), vec!["=".to_string()], 21, 20),
        ],
    );

    let tokens = Tokenizer::new(input, 0).tokenize();
    let mut input = Input::new(tokens);

    let mut parser = Parser::new(&mut input, language);
    let expr = parser.parse_expr(0);
    dbg!(input.peek());
    assert!(input.peek().is_none());
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