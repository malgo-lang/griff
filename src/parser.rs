use crate::{
    ast::{Exp, Id, Literal},
    lexer::{Lexer, Token, TokenKind},
};

pub struct Parser {
    tokens: Vec<Token>,
    cur: usize,
}

type ParseError = String;

const RESERVED: [&'static str; 3] = ["(", ")", ","];

impl Parser {
    pub fn new<'a>(lexer: Lexer<'a>) -> Parser {
        let tokens = lexer.lex();
        Parser { tokens, cur: 0 }
    }

    pub fn parse(mut self) -> Result<Exp, ParseError> {
        let exp = self.expression()?;
        if self.peek().kind != TokenKind::EOF {
            Err(format!("unexpected token: {:?}", self.tokens[self.cur]))
        } else {
            Ok(exp)
        }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.cur]
    }

    fn bump(&mut self) {
        self.cur += 1;
    }

    fn backtrack<F, O>(&mut self, f: F) -> Result<O, ParseError>
    where
        F: FnOnce(&mut Self) -> Result<O, ParseError>,
    {
        let cur = self.cur;
        let res = f(self);
        match res {
            Ok(o) => Ok(o),
            Err(e) => {
                self.cur = cur;
                Err(e)
            }
        }
    }

    /// expression := juxtaposition
    fn expression(&mut self) -> Result<Exp, ParseError> {
        self.juxtaposition()
    }

    /*
    FIXME: If we add a tuple syntax, we should add a special case for it here.
         tuple2 = "(" expression "," expression ")"
         atomic = tuple2 | ...
    `if e1 then (x, y) else e2` is parsed as `(RawApp if e1 (App then (Tuple x y)) else e2)`.
    But we want to parse it as `(RawApp if e1 then (Tuple x y) else e2)`.
    There are two options:
    1. Parse `suffix` as `juxtaposition` and then reconstruct to `App` later.
    2. `identifier` filter out symbols that can be appeared in `juxtaposition`.

    IMO, option 1 is better. Because it is more flexible.
    Option 1 needs one more pass to reconstruct (parse) `RawApp`, but we needs this pass anyway (`RawApp` is raw).

    Option 2 needs pre-parsing to inspect what can be appeared as symbol (not variable).
    Implementing this makes two more passes, one for `RawApp` and one for option 2.
    */

    /// juxtaposition := suffix suffix*
    fn juxtaposition(&mut self) -> Result<Exp, ParseError> {
        let expr = self.suffix()?;
        let mut exprs = vec![expr.clone()];
        while let Ok(e) = self.backtrack(Self::suffix) {
            dbg!(self.peek());
            exprs.push(e);
        }
        if exprs.len() == 1 {
            Ok(expr)
        } else {
            Ok(Exp::RawApp { exprs })
        }
    }

    /// suffix := atomic suffix_op
    fn suffix(&mut self) -> Result<Exp, ParseError> {
        let mut expr = self.atomic()?;
        while let Ok(builder) = self.backtrack(Self::suffix_op) {
            expr = builder(expr);
        }
        Ok(expr)
    }

    /// suffix_op := '(' sep_end_by(expression, ',') ')'
    fn suffix_op(&mut self) -> Result<Box<dyn Fn(Exp) -> Exp>, ParseError> {
        match &self.peek().kind {
            TokenKind::Symbol(s) if s == "(" => {
                self.bump();
                let mut args = vec![];
                while let Ok(e) = self.expression() {
                    args.push(e);
                    match &self.peek().kind {
                        TokenKind::Symbol(s) if s == ")" => break,
                        TokenKind::Symbol(s) if s == "," => self.bump(),
                        _ => return Err(format!("expected ',' or ')'")),
                    }
                }
                match &self.peek().kind {
                    TokenKind::Symbol(s) if s == ")" => {
                        self.bump();
                        Ok(Box::new(move |e| Exp::App {
                            fun: Box::new(e),
                            args: args.clone(),
                        }))
                    }
                    _ => Err(format!("expected ')'")),
                }
            }
            _ => Err("Expected operator".to_string()),
        }
    }

    /// atomic := identifier | literal | '(' expression ')'
    fn atomic(&mut self) -> Result<Exp, ParseError> {
        if let Ok(ident) = self.ident() {
            Ok(Exp::Ident(ident))
        // } else if let Ok(lit) = self.literal() {
        //     Ok(Exp::Literal(lit))
        // } else if let Ok(e) = self.parens() {
        //     Ok(e)
        } else {
            Err("Expected expression".to_string())
        }
    }

    fn ident(&mut self) -> Result<Id, ParseError> {
        fn is_ident(s: &str) -> bool {
            RESERVED.into_iter().all(|r| r != s)
        }
        if let TokenKind::Symbol(s) = self.peek().kind.clone() {
            if is_ident(s.as_str()) {
                self.bump();
                return Ok(Id::new(s.to_string()));
            }
        }
        Err("Expected identifier".to_string())
    }

    fn literal(&mut self) -> Result<Literal, ParseError> {
        todo!()
    }

    fn parens(&mut self) -> Result<Exp, ParseError> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_simple_juxtaposition() {
        let lexer = Lexer::new("a b c");
        let mut parser = Parser::new(lexer);
        let exp = parser.expression().unwrap();
        assert_eq!(
            exp,
            Exp::RawApp {
                exprs: vec![
                    Exp::Ident(Id::new("a".to_string())),
                    Exp::Ident(Id::new("b".to_string())),
                    Exp::Ident(Id::new("c".to_string())),
                ]
            }
        );
    }

    #[test]
    fn test_simple_application() {
        let lexer = Lexer::new("f(a, b)");
        let mut parser = Parser::new(lexer);
        let res = parser.expression().unwrap();
        assert_eq!(
            res,
            Exp::App {
                fun: Box::new(Exp::Ident(Id::new("f".to_string()))),
                args: vec![
                    Exp::Ident(Id::new("a".to_string())),
                    Exp::Ident(Id::new("b".to_string())),
                ]
            }
        );
    }

    #[test]
    fn test_application_chain() {
        let lexer = Lexer::new("f(a, b)(c, d)");
        let mut parser = Parser::new(lexer);
        let res = parser.expression().unwrap();
        assert_eq!(
            res,
            Exp::App {
                fun: Box::new(Exp::App {
                    fun: Box::new(Exp::Ident(Id::new("f".to_string()))),
                    args: vec![
                        Exp::Ident(Id::new("a".to_string())),
                        Exp::Ident(Id::new("b".to_string())),
                    ]
                }),
                args: vec![
                    Exp::Ident(Id::new("c".to_string())),
                    Exp::Ident(Id::new("d".to_string())),
                ]
            }
        );
    }
}
