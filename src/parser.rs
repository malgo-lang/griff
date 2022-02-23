use combine::{
    any, attempt, between, choice,
    error::Commit,
    many, many1, not_followed_by, optional,
    parser::{
        char::{char, digit, spaces},
        function::parser,
    },
    satisfy, satisfy_map, ParseError, Parser, Stream, StreamOnce,
};
use nom::character;

use crate::ast::{Exp, Id, Literal};

/*
 * expression := binary_operation
 *
 * binary_operation := prefix_operation_like (binary_operator prefix_operation_like)*
 * binary_operator := '+' | '-' | '*' | '/' | '%' | '^' | '==' | '!=' | '<' | '>' | '<=' | '>='
 *
 * prefix_operation_like := prefix_operation | suffix_operation_like
 *
 * prefix_operation := prefix_operator prefix_operation_like
 * prefix_operator := '!'
 *
 * suffix_operation_like := atomic_expression suffix_operator*
 * suffix_operator := '(' sep_end_by(expression, ',') ')'
 *
 * atomic_expression := literal | identifier | '(' expression ')'
 *
 * literal := integer | float | string | char
 * integer := [0-9]+
 * float := [0-9]+ '.' [0-9]+
 * string := '"' [^"]* '"'
 * char := '\'' [^'] '\''
 * identifier := [a-zA-Z_][a-zA-Z0-9_]*
 */

fn literal<Input>() -> impl Parser<Input, Output = Literal>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice((number_literal(), string_literal(), char_literal()))
}

fn number_literal<Input>() -> impl Parser<Input, Output = Literal>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice((attempt(int_literal()), float_literal()))
}

fn float_literal<Input>() -> impl Parser<Input, Output = Literal>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    float().map(|f| Literal::Float(f))
}

fn int_literal<Input>() -> impl Parser<Input, Output = Literal>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    integer().map(|i| Literal::Int(i))
}

fn string_literal<Input>() -> impl Parser<Input, Output = Literal>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    lex((
        char('"'),
        many(choice((
            satisfy(|c| c != '"' && c != '\\'),
            char('\\').with(choice((
                char('"'),
                char('\''),
                char('\\'),
                char('n'),
                char('r'),
                char('t'),
            ))),
        ))),
        char('"'),
    ))
    .map(|(_, s, _): (_, String, _)| Literal::String(s))
}

fn char_literal<Input>() -> impl Parser<Input, Output = Literal>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    lex((
        char('\''),
        choice((
            satisfy(|c| c != '"' && c != '\\'),
            char('\\').with(choice((
                char('"'),
                char('\''),
                char('\\'),
                char('n'),
                char('r'),
                char('t'),
            ))),
        )),
        char('\''),
    ))
    .map(|(_, c, _): (_, char, _)| Literal::Char(c))
}

// Parse a identifier.
fn identifier<Input>() -> impl Parser<Input, Output = Id>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    lex(raw_identifier()).expected("identifier")
}

fn raw_identifier<Input>() -> impl Parser<Input, Output = Id>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    (
        satisfy(|c: char| c.is_ascii_alphabetic() || c == '_'),
        many(satisfy(|c: char| c.is_ascii_alphanumeric() || c == '_')),
    )
        .map(|(head, tail): (char, String)| Id::new([head.into(), tail].concat()))
}

fn lex<Input, P>(p: P) -> impl Parser<Input, Output = P::Output>
where
    P: Parser<Input>,
    Input: Stream<Token = char>,
    <Input as StreamOnce>::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    p.skip(spaces())
}

fn integer<Input>() -> impl Parser<Input, Output = i64>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let i = many1(digit()).map(|s: String| {
        let mut n = 0;
        for c in s.chars() {
            n = n * 10 + (c as i64 - '0' as i64);
        }
        n
    });

    lex((
        optional(char('-'))
            .and(i)
            .map(|(sign, n)| if sign.is_some() { -n } else { n }),
        not_followed_by(choice((char('.'), char('e'), char('E')))),
    ))
    .map(|(n, _)| n)
    .expected("integer")
}

fn float<Input>() -> impl Parser<Input, Output = f64>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    let i = optional(char('-'))
        .and(many1(digit()).map(|s: String| {
            let mut n = 0;
            for c in s.chars() {
                n = n * 10 + (c as i64 - '0' as i64);
            }
            n
        }))
        .map(|(sign, n)| if sign.is_some() { -n } else { n })
        .map(|x| x as f64);
    let fractional = many(digit()).map(|digits: String| {
        let mut magnitude = 1.0;
        digits.chars().fold(0.0, |acc, d| {
            magnitude /= 10.0;
            match d.to_digit(10) {
                Some(d) => acc + (d as f64) * magnitude,
                None => panic!("Not a digit"),
            }
        })
    });

    let exp = satisfy(|c| c == 'e' || c == 'E').with(optional(char('-')).and(integer()));
    lex(i
        .and(optional(char('.')).with(fractional))
        .map(|(x, y)| if x >= 0.0 { x + y } else { x - y })
        .and(optional(exp))
        .map(|(n, exp_option)| match exp_option {
            Some((sign, e)) => {
                let e = if sign.is_some() { -e } else { e };
                n * 10_f64.powi(e as i32)
            }
            None => n,
        }))
    .expected("float")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifier() {
        let input = "abc";
        let result = identifier().parse(input);
        assert_eq!(result.unwrap(), (Id::new("abc".to_string()), ""));
    }

    #[test]
    fn test_literal() {
        let input = "123";
        let result = literal().parse(input);
        assert_eq!(result.unwrap(), (Literal::Int(123), ""));

        let input = "123.456";
        let result = literal().parse(input);
        assert_eq!(result.unwrap(), (Literal::Float(123.456), ""));

        let input = "\"abc\"";
        let result = literal().parse(input);
        assert_eq!(result.unwrap(), (Literal::String("abc".to_string()), ""));

        let input = "'a'";
        let result = literal().parse(input);
        assert_eq!(result.unwrap(), (Literal::Char('a'), ""));
    }
}
