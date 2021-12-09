use regex::Regex;

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    Symbol,
    Number,
}

impl TokenKind {
    /// Returns `true` if the token kind is [`Symbol`].
    ///
    /// [`Symbol`]: TokenKind::Symbol
    pub fn is_symbol(&self) -> bool {
        matches!(self, Self::Symbol)
    }

    /// Returns `true` if the token kind is [`Number`].
    ///
    /// [`Number`]: TokenKind::Number
    pub fn is_number(&self) -> bool {
        matches!(self, Self::Number)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub start: usize,
    pub end: usize,
}

impl Token {
    pub fn new(kind: TokenKind, text: String, start: usize, end: usize) -> Self {
        Self {
            kind,
            text,
            start,
            end,
        }
    }
    
    pub fn is_symbol(&self) -> bool {
        self.kind.is_symbol()
    }
    
    pub fn is_number(&self) -> bool {
        self.kind.is_number()
    }
}

pub struct Tokenizer<'s> {
    text: &'s str,
    position: usize,
}

impl<'s> Tokenizer<'s> {
    pub fn new(text: &'s str, position: usize) -> Self {
        Self { text, position }
    }

    pub fn tokenize(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while !self.is_at_end() {
            self.consume_whitespace();
            if self.is_at_end() {
                break;
            }
            let token = self.consume_token();
            tokens.push(token);
        }
        tokens
    }

    // Reads a character from the input.
    fn peek(&self) -> Option<char> {
        self.text[self.position..].chars().next()
    }

    // Consumes a character from the input.
    fn consume(&mut self) {
        self.position += self.peek().unwrap().len_utf8();
    }

    // Get the text starts at the current position.
    fn as_text(&self) -> &str {
        &self.text[self.position..]
    }

    // Returns `true` if the current position is at the end of the input.
    fn is_at_end(&self) -> bool {
        matches!(self.peek(), None)
    }

    // Consumes whitespace characters and comments.
    fn consume_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.consume();
            } else if c == '#' {
                while let Some(c) = self.peek() {
                    if c == '\n' {
                        break;
                    }
                    self.consume();
                }
            } else {
                break;
            }
        }
    }

    fn consume_token(&mut self) -> Token {
        if let Some(token) = self.consume_symbol() {
            return token;
        }
        if let Some(token) = self.consume_number() {
            return token;
        }
        panic!("unexpected character");
    }

    fn consume_symbol(&mut self) -> Option<Token> {
        let symbol_re =
            Regex::new(r"^\p{XID_Start}\p{XID_Continue}*|^(\p{Punctuation}|\p{Symbol})+").unwrap();
        if let Some(captures) = symbol_re.captures(self.as_text()) {
            let text: String = captures.get(0).unwrap().as_str().to_string();
            let start = self.position;
            let end = start + text.len();
            self.position = end;
            return Some(Token::new(TokenKind::Symbol, text, start, end));
        }
        return None;
    }

    fn consume_number(&mut self) -> Option<Token> {
        let number_re = Regex::new(r"^[0-9]+").unwrap();
        if let Some(captures) = number_re.captures(self.as_text()) {
            let text: String = captures.get(0).unwrap().as_str().to_string();
            let start = self.position;
            let end = start + text.len();
            self.position = end;
            return Some(Token::new(TokenKind::Number, text, start, end));
        }
        return None;
    }
}

#[test]
fn test_simple_symbols() {
    let tokenizer = Tokenizer::new("hello world", 0);
    let tokens = tokenizer.tokenize();
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenKind::Symbol, "hello".to_string(), 0, 5),
            Token::new(TokenKind::Symbol, "world".to_string(), 6, 11),
        ]
    );
}

#[test]
fn test_simple_numbers() {
    let tokenizer = Tokenizer::new("123 456", 0);
    let tokens = tokenizer.tokenize();
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenKind::Number, "123".to_string(), 0, 3),
            Token::new(TokenKind::Number, "456".to_string(), 4, 7),
        ]
    );
}

#[test]
fn test_arith_expression() {
    let tokenizer = Tokenizer::new("1 + 2 * 3", 0);
    let tokens = tokenizer.tokenize();
    assert_eq!(
        tokens,
        vec![
            Token::new(TokenKind::Number, "1".to_string(), 0, 1),
            Token::new(TokenKind::Symbol, "+".to_string(), 2, 3),
            Token::new(TokenKind::Number, "2".to_string(), 4, 5),
            Token::new(TokenKind::Symbol, "*".to_string(), 6, 7),
            Token::new(TokenKind::Number, "3".to_string(), 8, 9),
        ]
    );
}

#[test]
fn test_if_expression() {
    let tokenizer = Tokenizer::new("if (x < y) { x }", 0);
    let tokens = tokenizer.tokenize();
    use TokenKind::*;
    assert_eq!(
        tokens,
        vec![
            Token::new(Symbol, "if".to_string(), 0, 2),
            Token::new(Symbol, "(".to_string(), 3, 4),
            Token::new(Symbol, "x".to_string(), 4, 5),
            Token::new(Symbol, "<".to_string(), 6, 7),
            Token::new(Symbol, "y".to_string(), 8, 9),
            Token::new(Symbol, ")".to_string(), 9, 10),
            Token::new(Symbol, "{".to_string(), 11, 12),
            Token::new(Symbol, "x".to_string(), 13, 14),
            Token::new(Symbol, "}".to_string(), 15, 16)
        ]
    );
}
