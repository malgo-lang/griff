use std::str::CharIndices;

use regex::Regex;

const SYMBOL_REGEX: &str = r"^\p{Open_Punctuation}|^\p{Close_Punctuation}|^(\p{XID_Start}|_)\p{XID_Continue}*|^(\p{Punctuation}|\p{Symbol})+";

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Symbol(String),
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
    Comment(String),
    EOF,
}

pub struct Lexer<'a> {
    source: &'a str,
    // Current cursor
    iter: CharIndices<'a>,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            iter: source.char_indices(),
            line: 0,
            column: 0,
        }
    }

    pub fn lex(mut self) -> Vec<Token> {
        let mut tokens = vec![];
        while let Some(token) = self.next() {
            tokens.push(token);
        }
        tokens.push(Token {
            kind: TokenKind::EOF,
            line: self.line,
            column: self.column,
        });
        tokens
    }

    #[inline]
    fn as_str(&self) -> &'a str {
        self.iter.as_str()
    }

    #[inline]
    fn cur(&mut self) -> Option<char> {
        self.iter.clone().next().map(|(_, c)| c)
    }

    #[inline]
    fn peek(&mut self) -> Option<char> {
        self.iter.clone().nth(1).map(|(_, c)| c)
    }

    #[inline]
    fn bump(&mut self) {
        match self.cur() {
            Some(c) => {
                if c == '\n' {
                    self.line += 1;
                    self.column = 0;
                } else {
                    self.column += 1;
                }
            }
            None => {}
        }
        match self.iter.next() {
            Some(_) => {}
            None => panic!("bump past EOF"),
        }
    }

    #[inline]
    fn bump_n(&mut self, n: usize) {
        for _ in 0..n {
            self.bump();
        }
    }

    // Skip whitespaces
    fn whitespace(&mut self) {
        while let Some(c) = self.cur() {
            if c.is_whitespace() {
                self.bump();
            } else {
                break;
            }
        }
    }

    fn next(&mut self) -> Option<Token> {
        if let None = self.cur() {
            return None;
        }
        self.whitespace();

        let symbol_re = Regex::new(SYMBOL_REGEX).unwrap();

        if symbol_re.is_match(self.as_str()) {
            Some(self.symbol())
        } else {
            match self.cur() {
                Some(c) if c.is_ascii_digit() => Some(self.number()),
                _ => None,
            }
        }
    }

    // Tokenize an ident. self.as_str() must match SYMBOL_REGEX.
    fn symbol(&mut self) -> Token {
        let line = self.line;
        let column = self.column;
        let symbol_re = Regex::new(SYMBOL_REGEX).unwrap();
        if let Some(captures) = symbol_re.captures(self.as_str()) {
            let text = captures.get(0).unwrap().as_str().to_string();
            self.bump_n(text.chars().count());
            Token {
                kind: TokenKind::Symbol(text),
                line,
                column,
            }
        } else {
            panic!("symbol not found")
        }
    }

    // Tokenize an number. First char must be a digit.
    fn number(&mut self) -> Token {
        let line = self.line;
        let column = self.column;
        let mut s = String::new();
        let mut is_float = false;
        while let Some(c) = self.cur() {
            if c.is_ascii_digit() {
                s.push(c);
                self.bump();
            } else {
                break;
            }
        }
        if let Some('.') = self.cur() {
            s.push('.');
            self.bump();
            is_float = true;
            while let Some(c) = self.cur() {
                if c.is_ascii_digit() {
                    s.push(c);
                    self.bump();
                } else {
                    break;
                }
            }
        }
        if let Some(c) = self.cur() {
            if c == 'e' || c == 'E' {
                s.push(c);
                self.bump();
                is_float = true;
                if let Some('+') = self.cur() {
                    s.push('+');
                    self.bump();
                }
                if let Some('-') = self.cur() {
                    s.push('-');
                    self.bump();
                }
                while let Some(c) = self.cur() {
                    if c.is_ascii_digit() {
                        s.push(c);
                        self.bump();
                    } else {
                        break;
                    }
                }
            }
        }
        Token {
            kind: if is_float {
                TokenKind::Float(s.parse().unwrap())
            } else {
                TokenKind::Int(s.parse().unwrap())
            },
            line,
            column,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbol() {
        let mut lexer = Lexer::new("abc");
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("abc".to_string()),
                line: 0,
                column: 0,
            })
        );
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn some_symbols() {
        let mut lexer = Lexer::new("abc def Ghi _123");
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("abc".to_string()),
                line: 0,
                column: 0,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("def".to_string()),
                line: 0,
                column: 4,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("Ghi".to_string()),
                line: 0,
                column: 8,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("_123".to_string()),
                line: 0,
                column: 12,
            })
        );
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn punctuators() {
        let mut lexer = Lexer::new("<= 《 ( )");
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("<=".to_string()),
                line: 0,
                column: 0,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("《".to_string()),
                line: 0,
                column: 3,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("(".to_string()),
                line: 0,
                column: 5,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol(")".to_string()),
                line: 0,
                column: 7,
            })
        );
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn split_symbols() {
        let mut lexer = Lexer::new("f(x, y⸡Γ)");
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("f".to_string()),
                line: 0,
                column: 0,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("(".to_string()),
                line: 0,
                column: 1,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("x".to_string()),
                line: 0,
                column: 2,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol(",".to_string()),
                line: 0,
                column: 3,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("y".to_string()),
                line: 0,
                column: 5,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("⸡".to_string()),
                line: 0,
                column: 6,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("Γ".to_string()),
                line: 0,
                column: 7,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol(")".to_string()),
                line: 0,
                column: 8,
            })
        );
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn japanese_glyphs() {
        let mut lexer =
            Lexer::new("あいうえお　がぎぐげご　サシスセソ　ダヂヅデド　平仮名　片仮名");
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("あいうえお".to_string()),
                line: 0,
                column: 0,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("がぎぐげご".to_string()),
                line: 0,
                column: 6,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("サシスセソ".to_string()),
                line: 0,
                column: 12,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("ダヂヅデド".to_string()),
                line: 0,
                column: 18,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("平仮名".to_string()),
                line: 0,
                column: 24,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Symbol("片仮名".to_string()),
                line: 0,
                column: 28,
            })
        );
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn integer() {
        let mut lexer = Lexer::new("123");
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Int(123),
                line: 0,
                column: 0,
            })
        );
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn float() {
        let mut lexer = Lexer::new("123.456");
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Float(123.456),
                line: 0,
                column: 0,
            })
        );
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn float_exp() {
        let mut lexer = Lexer::new("123.456e7");
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Float(123.456e7),
                line: 0,
                column: 0,
            })
        );
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn some_numbers() {
        let mut lexer = Lexer::new("123.456e7 123\n123.456\t123.456e-7");
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Float(123.456e7),
                line: 0,
                column: 0,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Int(123),
                line: 0,
                column: 10,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Float(123.456),
                line: 1,
                column: 0,
            })
        );
        assert_eq!(
            lexer.next(),
            Some(Token {
                kind: TokenKind::Float(123.456e-7),
                line: 1,
                column: 8,
            })
        );
        assert_eq!(lexer.next(), None);
    }
}
