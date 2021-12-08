// https://zenn.dev/pandaman64/books/pratt-parsing
use std::fmt;

#[derive(Debug)]
pub enum SExpr {
    Atom(String),
    List(Vec<SExpr>),
}

impl fmt::Display for SExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SExpr::Atom(s) => write!(f, "{}", s),
            SExpr::List(l) => {
                let mut iter = l.iter();
                write!(f, "(")?;
                if let Some(head) = iter.next() {
                    write!(f, "{}", head)?;
                }
                for rest in iter {
                    write!(f, " {}", rest)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[test]
fn test_sexpr_print() {
    let e = SExpr::List(vec![
        SExpr::Atom("+".into()),
        SExpr::Atom("1".into()),
        SExpr::List(vec![
            SExpr::Atom("+".into()),
            SExpr::Atom("2".into()),
            SExpr::Atom("3".into()),
        ]),
    ]);

    assert_eq!(e.to_string(), "(+ 1 (+ 2 3))");
}

#[derive(Debug)]
pub struct Input<'s> {
    text: &'s str,
    position: usize,
}

impl<'s> Input<'s> {
    pub fn new(text: &'s str) -> Self {
        Self { text, position: 0 }
    }

    // read one character
    pub fn peek(&self) -> Option<char> {
        self.text[self.position..].chars().next()
    }

    pub fn bump(&mut self) {
        self.position += self.peek().unwrap().len_utf8();
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LeadingOpKind {
    Prefix { right_bp: u16 },
    Paren,
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug)]
pub struct Operator<K> {
    kind: K,
    name: String,
    symbols: Vec<char>,
}

pub type LeadingOp = Operator<LeadingOpKind>;
pub type FollowingOp = Operator<FollowingOpKind>;

pub fn prefix(name: String, symbols: Vec<char>, right_bp: u16) -> LeadingOp {
    LeadingOp {
        kind: LeadingOpKind::Prefix { right_bp },
        name,
        symbols,
    }
}

pub fn paren(name: String, symbols: Vec<char>) -> LeadingOp {
    LeadingOp {
        kind: LeadingOpKind::Paren,
        name,
        symbols,
    }
}

pub fn postfix(name: String, symbols: Vec<char>, left_bp: u16) -> FollowingOp {
    FollowingOp {
        kind: FollowingOpKind::Postfix { left_bp },
        name,
        symbols,
    }
}

pub fn infix(name: String, symbols: Vec<char>, left_bp: u16, right_bp: u16) -> FollowingOp {
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

pub fn parse_atom(input: &mut Input<'_>) -> SExpr {
    match input.peek().unwrap() {
        c if c.is_ascii_digit() => {
            input.bump(); // consume a digit from input
            SExpr::Atom(c.into())
        }
        c => panic!("expected an atom, got {}", c),
    }
}

pub fn parse_expr(language: &Language, input: &mut Input<'_>, min_bp: u16) -> SExpr {
    let mut leading_expr = {
        let mut expr = None;
        let c = input.peek().unwrap();

        for leading_operator in language.leading_operators.iter() {
            // match a leading operator
            if leading_operator.symbols[0] == c {
                input.bump();
                let mut children = vec![SExpr::Atom(leading_operator.name.clone())];

                // 記号の内側部分
                for symbol in leading_operator.symbols[1..].iter() {
                    let inner_expr = parse_expr(language, input, 0);
                    children.push(inner_expr);

                    assert_eq!(input.peek().unwrap(), *symbol);
                    input.bump();
                }

                // prefix演算子の場合は、後ろに続く式をパース
                if let LeadingOpKind::Prefix { right_bp } = leading_operator.kind {
                    let following_expr = parse_expr(language, input, right_bp);
                    children.push(following_expr);
                }

                expr = Some(SExpr::List(children));
            }
        }

        match expr {
            Some(expr) => expr,
            // マッチする先行演算子がない場合は、atomをパース
            None => parse_atom(input),
        }
    };

    'main: loop {
        match input.peek() {
            None => return leading_expr,
            Some(c) => {
                for following_operator in language.following_operators.iter() {
                    // 後続演算子にマッチ
                    if following_operator.symbols[0] == c {
                        // 演算子の優先順位が足りない場合はやめる
                        if following_operator.kind.left_bp() <= min_bp {
                            return leading_expr;
                        }

                        input.bump();
                        let mut children =
                            vec![SExpr::Atom(following_operator.name.clone()), leading_expr];

                        // 記号の内側部分
                        for symbol in following_operator.symbols[1..].iter() {
                            let inner_expr = parse_expr(language, input, 0);
                            children.push(inner_expr);

                            assert_eq!(input.peek().unwrap(), *symbol);
                            input.bump();
                        }

                        // infix演算子の場合は後ろに続く式をパース
                        if let FollowingOpKind::Infix { right_bp, .. } = following_operator.kind {
                            let following_expr = parse_expr(language, input, right_bp);
                            children.push(following_expr);
                        }

                        leading_expr = SExpr::List(children);
                        continue 'main; // 残りの後続演算子を再帰的にパース
                    }
                }

                return leading_expr;
            }
        }
    }
}

#[cfg(test)]
fn complete_parse(input: &str, expected: &str) {
    let language = Language::new(
        vec![
            prefix("-".into(), vec!['-'], 51),
            prefix("if-then-else".into(), vec!['I', 'T', 'E'], 41),
            paren("paren".into(), vec!['(', ')']),
        ],
        vec![
            postfix("?".into(), vec!['?'], 20),
            postfix("subscript".into(), vec!['[', ']'], 100),
            infix("+".into(), vec!['+'], 50, 51),
            infix("-".into(), vec!['-'], 50, 51),
            infix("*".into(), vec!['*'], 80, 81),
            infix("=".into(), vec!['='], 21, 20),
        ],
    );

    let mut input = Input::new(input);
    let expr = parse_expr(&language, &mut input, 0);
    assert_eq!(expr.to_string(), expected);
    assert!(input.peek().is_none());
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
    complete_parse("7+-8", "(+ 7 (- 8))");
}

#[test]
fn test_different_position() {
    complete_parse("1--2", "(- 1 (- 2))")
}

#[test]
fn test_complex() {
    complete_parse(
        "1=2=I(3)T(4)E(5[6])",
        "(= 1 (= 2 (if-then-else (paren 3) (paren 4) (paren (subscript 5 6)))))",
    )
}
fn main() {
    println!("Hello, world!");
}
