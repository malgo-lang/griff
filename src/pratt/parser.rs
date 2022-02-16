use crate::{
    sexpr::{Atom, SExpr},
    pratt::token::Token,
};

// Based on Pratt's parser
// Ref: https://zenn.dev/pandaman64/books/pratt-parsing

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
    List(Box<Part>),
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

#[derive(Debug, Clone)]
pub enum ParseError {
    Unexpected { expected: Part, actual: Token },
    EndOfInput { expected: Part },
}

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
    language: Language,
    keywords: Vec<String>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, position: usize, language: Language) -> Self {
        let mut keywords: Vec<String> = vec![];
        for op in language.leading_operators.iter() {
            for part in op.parts.iter() {
                if let Part::Symbol(keyword) = part {
                    keywords.push(keyword.to_string())
                }
            }
        }
        for op in language.following_operators.iter() {
            for part in op.parts.iter() {
                if let Part::Symbol(keyword) = part {
                    keywords.push(keyword.to_string())
                }
            }
        }
        Self {
            tokens,
            position,
            language,
            keywords,
        }
    }

    // read one character
    fn peek(&self) -> Result<&Token, ParseError> {
        match self.tokens.get(self.position) {
            Some(token) => Ok(token),
            None => Err(ParseError::EndOfInput {
                expected: Part::Expr,
            }),
        }
    }

    fn consume(&mut self) {
        self.position += 1;
    }

    // Parse an atom.
    fn parse_atom(&mut self) -> Result<SExpr, ParseError> {
        match self.peek() {
            Ok(token) if token.is_symbol() => {
                if self.keywords.contains(&token.text) {
                    Err(ParseError::Unexpected {
                        expected: Part::Atom,
                        actual: token.clone(),
                    })
                } else {
                    let name = token.text.clone();
                    self.consume(); // consume a symbol from input
                    Ok(SExpr::Atom(Atom::Symbol(name)))
                }
            }
            Ok(token) if token.is_number() => {
                let number = token.text.clone();
                self.consume(); // consume a number from input
                Ok(SExpr::Atom(Atom::Natural(number.parse().unwrap())))
            }
            Ok(token) => Err(ParseError::Unexpected {
                expected: Part::Atom,
                actual: token.clone(),
            }),
            Err(err) => Err(err),
        }
    }

    pub fn parse_expr(&mut self, min_bp: u16) -> Result<SExpr, ParseError> {
        let mut leading_expr = {
            let mut expr = None;
            let token = self.peek().unwrap();
            let text = &token.text;

            let start_position = self.position;

            for leading_operator in self.peek_leading_operators(text) {
                // match a leading operator
                self.consume();
                match self.parse_leading_operator(leading_operator) {
                    Ok(e) => {
                        expr = Some(e);
                    }
                    Err(_) => {
                        self.position = start_position;
                        continue;
                    }
                }
                break;
            }

            match expr {
                Some(expr) => Ok(expr),
                // マッチする先行演算子がない場合は、atomをパース
                None => self.parse_atom(),
            }
        }?;

        'following: loop {
            if let Ok(token) = self.peek() {
                let start_position = self.position;
                for following_operator in self.peek_following_operators(&token.text) {
                    // If the precedence of the following operator is greater than the minimum, skip this operator.
                    if following_operator.kind.left_bp() <= min_bp {
                        continue;
                    }

                    self.consume();
                    match self.parse_following_operator(following_operator, leading_expr.clone()) {
                        Ok(e) => {
                            leading_expr = e;
                            continue 'following; // continue to the next following operator
                        }
                        Err(_) => {
                            self.position = start_position;
                            continue;
                        }
                    }
                }
            }
            return Ok(leading_expr);
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

    fn parse_part(&mut self, part: &Part) -> Result<Option<SExpr>, ParseError> {
        match part {
            Part::Atom => {
                let atom = self.parse_atom()?;
                Ok(Some(atom))
            }
            Part::Expr => {
                let expr = self.parse_expr(0)?;
                Ok(Some(expr))
            }
            Part::Symbol(name) => match self.peek() {
                Ok(token) if token.text == *name => {
                    self.consume();
                    Ok(None)
                }
                _ => Err(ParseError::EndOfInput {
                    expected: Part::Symbol(name),
                }),
            },
            Part::List(pattern) => {
                let mut list = vec![];
                while let Ok(e) = self.parse_part(pattern) {
                    if let Some(e) = e {
                        list.push(e)
                    }
                }
                Ok(Some(SExpr::List(list)))
            }
        }
    }

    fn parse_leading_operator(&mut self, leading_operator: LeadingOp) -> Result<SExpr, ParseError> {
        let mut children = vec![SExpr::Atom(Atom::Symbol(leading_operator.name.clone()))];

        // 記号の内側部分
        for part in leading_operator.parts[1..].iter() {
            match self.parse_part(part) {
                Ok(Some(e)) => children.push(e),
                Ok(None) => continue,
                Err(err) => return Err(err),
            }
        }

        // prefix演算子の場合は、後ろに続く式をパース
        if let LeadingOpKind::Prefix { right_bp } = leading_operator.kind {
            let following_expr = self.parse_expr(right_bp)?;
            children.push(following_expr);
        }

        Ok(SExpr::List(children))
    }

    fn parse_following_operator(
        &mut self,
        following_operator: FollowingOp,
        leading_expr: SExpr,
    ) -> Result<SExpr, ParseError> {
        let mut children = vec![
            SExpr::Atom(Atom::Symbol(following_operator.name.clone())),
            leading_expr,
        ];

        // 記号の内側部分
        for part in following_operator.parts[1..].iter() {
            match self.parse_part(part) {
                Ok(Some(e)) => children.push(e),
                Ok(None) => continue,
                Err(err) => return Err(err),
            }
        }

        // If the following operator is a infix operator, parse the following expression.
        if let FollowingOpKind::Infix { right_bp, .. } = following_operator.kind {
            let following_expr = self.parse_expr(right_bp)?;
            children.push(following_expr);
        }

        Ok(SExpr::List(children))
    }
}
