use std::f32::consts::E;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until, take_while_m_n},
    character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1, none_of, one_of},
    combinator::{map, map_opt, map_res, opt, recognize, value, verify},
    error::{Error, ErrorKind, FromExternalError, ParseError},
    multi::{fold_many0, many0, many1, separated_list0, separated_list1},
    sequence::{delimited, separated_pair, tuple},
    sequence::{pair, preceded, terminated},
    IResult, InputLength, Parser,
};

use crate::ast::{Exp, Id, Literal, Pat, Type};

fn parse_single_type<'a>(input: &'a str) -> IResult<&'a str, Type> {
    ws(alt((
        // ident
        map(identifier, |id| {
            Type::Ident(Id {
                name: id.to_string(),
            })
        }),
        // tuple
        map(
            delimited(
                ws(char('(')),
                separated_list_with(parse_type, ws(char(',')), parse_type, ws(char(','))),
                ws(char(')')),
            ),
            Type::Tuple,
        ),
        // record
        map(
            delimited(
                ws(char('{')),
                separated_list0(ws(char(',')), parse_record_field(parse_type)),
                ws(char('}')),
            ),
            |fields| Type::Record(fields),
        ),
        // block
        map(delimited(ws(char('{')), parse_type, ws(char('}'))), |ty| {
            Type::Block(Box::new(ty))
        }),
        // parens
        delimited(ws(char('(')), parse_type, ws(char(')'))),
    )))(input)
}

fn parse_record_field<'a, F: 'a, O, E: 'a + ParseError<&'a str>>(
    parse_value: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, (Id, O), E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    map(
        tuple((identifier, ws(char(':')), parse_value)),
        |(label, _, value)| {
            (
                Id {
                    name: label.to_string(),
                },
                value,
            )
        },
    )
}

fn parse_type_application<'a>(input: &'a str) -> IResult<&'a str, Type> {
    ws(alt((
        // application
        map(
            tuple((parse_single_type, many1(parse_single_type))),
            |(fun, args)| Type::App {
                fun: Box::new(fun),
                args,
            },
        ),
        parse_single_type,
    )))(input)
}

fn parse_type<'a>(input: &'a str) -> IResult<&'a str, Type> {
    alt((
        // arrow
        map(
            tuple((parse_type_application, ws(tag("->")), parse_type)),
            |(dom, _, cod)| Type::Arr {
                dom: Box::new(dom),
                cod: Box::new(cod),
            },
        ),
        parse_type_application,
    ))(input)
}

#[test]
fn test_parse_type() {
    let data = "Int";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::Ident(Id {
                name: "Int".to_string()
            })
        ))
    );

    let data = "(Int, Int)";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::Tuple(vec![
                Type::Ident(Id {
                    name: "Int".to_string()
                }),
                Type::Ident(Id {
                    name: "Int".to_string()
                }),
            ])
        ))
    );

    let data = "List Int";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::App {
                fun: Box::new(Type::Ident(Id {
                    name: "List".to_string()
                })),
                args: vec![Type::Ident(Id {
                    name: "Int".to_string()
                })],
            }
        ))
    );

    let data = "Either Int String";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::App {
                fun: Box::new(Type::Ident(Id {
                    name: "Either".to_string()
                })),
                args: vec![
                    Type::Ident(Id {
                        name: "Int".to_string()
                    }),
                    Type::Ident(Id {
                        name: "String".to_string()
                    }),
                ],
            }
        ))
    );

    let data = "Either (List Int) (List String)";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::App {
                fun: Box::new(Type::Ident(Id {
                    name: "Either".to_string()
                })),
                args: vec![
                    Type::App {
                        fun: Box::new(Type::Ident(Id {
                            name: "List".to_string()
                        })),
                        args: vec![Type::Ident(Id {
                            name: "Int".to_string()
                        })],
                    },
                    Type::App {
                        fun: Box::new(Type::Ident(Id {
                            name: "List".to_string()
                        })),
                        args: vec![Type::Ident(Id {
                            name: "String".to_string()
                        })],
                    },
                ],
            }
        ))
    );

    let data = "Int -> Int";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::Arr {
                dom: Box::new(Type::Ident(Id {
                    name: "Int".to_string()
                })),
                cod: Box::new(Type::Ident(Id {
                    name: "Int".to_string()
                })),
            }
        ))
    );

    let data = "Int -> Int -> Int";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::Arr {
                dom: Box::new(Type::Ident(Id {
                    name: "Int".to_string()
                })),
                cod: Box::new(Type::Arr {
                    dom: Box::new(Type::Ident(Id {
                        name: "Int".to_string()
                    })),
                    cod: Box::new(Type::Ident(Id {
                        name: "Int".to_string()
                    })),
                }),
            }
        ))
    );

    let data = "{ x: Int, y: String }";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::Record(vec![
                (
                    Id {
                        name: "x".to_string()
                    },
                    Type::Ident(Id {
                        name: "Int".to_string()
                    })
                ),
                (
                    Id {
                        name: "y".to_string()
                    },
                    Type::Ident(Id {
                        name: "String".to_string()
                    })
                ),
            ])
        ))
    );

    let data = "{ a }";
    let result = parse_type(data);
    assert_eq!(
        result,
        Ok((
            "",
            Type::Block(Box::new(Type::Ident(Id {
                name: "a".to_string()
            })))
        ))
    );
}

fn parse_single_exp<'a>(input: &'a str) -> IResult<&'a str, Exp> {
    ws(alt((
        parse_ident,
        map(ws(terminated(parse_literal, tag("#"))), |lit| {
            Exp::Unboxed(lit)
        }),
        map(ws(parse_literal), |lit| Exp::Boxed(lit)),
        delimited(ws(char('(')), parse_exp, ws(char(')'))),
    )))(input)
}

fn parse_exp<'a>(input: &'a str) -> IResult<&'a str, Exp> {
    alt((
        map(
            tuple((parse_single_exp, many1(parse_single_exp))),
            |(head, tail)| Exp::App {
                fun: Box::new(head),
                args: tail,
            },
        ),
        parse_single_exp,
    ))(input)
}

#[test]
fn test_parse_exp() {
    let data = "f 1 2";
    let result = parse_exp(data);
    assert_eq!(
        result,
        Ok((
            "",
            Exp::App {
                fun: Box::new(Exp::Ident(Id {
                    name: "f".to_string()
                })),
                args: vec![Exp::Boxed(Literal::Int(1)), Exp::Boxed(Literal::Int(2))],
            }
        ))
    );
}

fn parse_single_pat<'a>(input: &'a str) -> IResult<&'a str, Pat> {
    alt((
        map(ws(char('_')), |_| Pat::Wildcard),
        map(ws(identifier), |id| {
            Pat::Ident(Id {
                name: id.to_string(),
            })
        }),
        map(
            delimited(
                ws(char('(')),
                separated_list_with(parse_pat, ws(char(',')), parse_pat, ws(char(','))),
                ws(char(')')),
            ),
            Pat::Tuple,
        ),
    ))(input)
}

fn separated_list_with<'a, O, O2, E, F1, G1, F2, G2>(
    first_item: F1,
    first_sep: G1,
    rest_item: F2,
    rest_sep: G2,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>, E>
where
    F1: Parser<&'a str, O, E>,
    G1: Parser<&'a str, O2, E>,
    F2: Parser<&'a str, O, E>,
    G2: Parser<&'a str, O2, E>,
    E: ParseError<&'a str>,
{
    map(
        separated_pair(first_item, first_sep, separated_list1(rest_sep, rest_item)),
        |(x, xs)| vec![x].into_iter().chain(xs).collect(),
    )
}

fn parse_pat<'a>(input: &'a str) -> IResult<&'a str, Pat> {
    alt((
        map(
            tuple((identifier, many1(parse_single_pat))),
            |(head, tail)| Pat::App {
                fun: Id {
                    name: head.to_string(),
                },
                args: tail,
            },
        ),
        parse_single_pat,
    ))(input)
}

#[test]
fn test_parse_pat() {
    let data = "Nil";
    let result = parse_pat(data);
    assert_eq!(
        result,
        Ok((
            "",
            Pat::Ident(Id {
                name: "Nil".to_string()
            })
        ))
    );

    let data = "Cons x xs";
    let result = parse_pat(data);
    assert_eq!(
        result,
        Ok((
            "",
            Pat::App {
                fun: Id {
                    name: "Cons".to_string()
                },
                args: vec![
                    Pat::Ident(Id {
                        name: "x".to_string()
                    }),
                    Pat::Ident(Id {
                        name: "xs".to_string()
                    }),
                ],
            }
        ))
    );

    let data = "(x, y, z)";
    let result = parse_pat(data);
    assert_eq!(
        result,
        Ok((
            "",
            Pat::Tuple(vec![
                Pat::Ident(Id {
                    name: "x".to_string()
                }),
                Pat::Ident(Id {
                    name: "y".to_string()
                }),
                Pat::Ident(Id {
                    name: "z".to_string()
                }),
            ])
        ))
    );
}

fn parse_ident<'a>(input: &'a str) -> IResult<&'a str, Exp> {
    ws(map(identifier, |id| {
        Exp::Ident(Id {
            name: id.to_string(),
        })
    }))(input)
}

#[test]
fn test_parse_ident() {
    let data = "x";
    let result = parse_exp(data);
    assert_eq!(
        result,
        Ok((
            "",
            Exp::Ident(Id {
                name: "x".to_string()
            })
        ))
    );

    let data = "x y";
    let result = parse_exp(data);
    assert_eq!(
        result,
        Ok((
            "y",
            Exp::Ident(Id {
                name: "x".to_string()
            })
        ))
    );
}

pub fn parse_literal<'a>(input: &'a str) -> IResult<&'a str, Literal> {
    alt((
        map(terminated(float, tag("f32")), |number_str| {
            Literal::Float32(number_str.parse::<f32>().unwrap())
        }),
        map(terminated(float, tag("f64")), |number_str| {
            Literal::Float64(number_str.parse::<f64>().unwrap())
        }),
        map(float, |number_str| {
            Literal::Float(number_str.parse::<f64>().unwrap())
        }),
        map(terminated(decimal, tag("i32")), |number_str| {
            Literal::Int32(number_str.parse::<i32>().unwrap())
        }),
        map(terminated(decimal, tag("i64")), |number_str| {
            Literal::Int64(number_str.parse::<i64>().unwrap())
        }),
        map(decimal, |number_str| {
            Literal::Int(number_str.parse::<i64>().unwrap())
        }),
        map(
            delimited(
                char('\''),
                alt((parse_escaped_char, none_of("\'"))),
                char('\''),
            ),
            Literal::Char,
        ),
        map(parse_string, Literal::String),
    ))(input)
}

#[test]
fn test_parse_literal() {
    let data = "123i32";
    let result = parse_literal(data);
    assert_eq!(result, Ok(("", Literal::Int32(123))));

    let data = "3.14f32";
    let result = parse_literal(data);
    assert_eq!(result, Ok(("", Literal::Float32(3.14))));

    let data = "314E-2f64";
    let result = parse_literal(data);
    assert_eq!(result, Ok(("", Literal::Float64(314E-2))));

    let data = "'a'";
    let result = parse_literal(data);
    assert_eq!(result, Ok(("", Literal::Char('a'))));

    let data = "'\\n'";
    let result = parse_literal(data);
    assert_eq!(result, Ok(("", Literal::Char('\n'))));

    let data = "'\\u{1F602}'";
    let result = parse_literal(data);
    assert_eq!(result, Ok(("", Literal::Char('\u{1F602}'))));

    let data = "\"Hello, world!\"";
    let result = parse_literal(data);
    assert_eq!(
        result,
        Ok(("", Literal::String("Hello, world!".to_string())))
    );
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn ws<'a, O, E, F>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
    E: ParseError<&'a str>,
{
    delimited(multispace0, inner, multispace0)
}

fn peol_comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    value(
        (), // Output is thrown away.
        pair(tag("--"), is_not("\n\r")),
    )(i)
}

// TODO: support nested comments
fn pinline_comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
    value(
        (), // Output is thrown away.
        tuple((tag("{-"), take_until("-}"), tag("-}"))),
    )(i)
}

/// Parses a identifier that may start with a letter or an underscore and may contain underscores, letters and numbers.
pub fn identifier<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

// A string is:
// - Enclosed by double quotes
// - Can contain any raw unescaped code point besides \ and "
// - Matches the following escape sequences: \b, \f, \n, \r, \t, \", \\, \/
// - Matches code points like Rust: \u{XXXX}, where XXXX can be up to 6
//   hex characters
// - an escape followed by whitespace consumes all whitespace between the
//   escape and the next non-whitespace character

/// Parse a unicode sequence, of the form u{XXXX}, where XXXX is 1 to 6
/// hexadecimal numerals. We will combine this later with parse_escaped_char
/// to parse sequences like \u{00AC}.
fn parse_unicode<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    // `parse_hex` parses between 1 and 6 hexadecimal numerals.
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

    // It parses u{XXXX}.
    let parse_delimited_hex = preceded(
        char('u'),
        // It parses {XXXX}, where XXXX is 1 to 6 hex numerals, and returns XXXX
        delimited(char('{'), parse_hex, char('}')),
    );

    // We take the hex bytes from parse_hex and attempt to convert them to a u32.
    let parse_u32 = map_res(parse_delimited_hex, move |hex| u32::from_str_radix(hex, 16));

    // Because not all u32 values are valid unicode code points, we have to fallibly
    // convert to char with from_u32.
    map_opt(parse_u32, |value| std::char::from_u32(value))(input)
}

/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
fn parse_escaped_char<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    preceded(
        char('\\'),
        alt((
            parse_unicode,
            // It looks for the marker characters (n, r, t, etc) and
            // returns the matching character (\n, \r, \t, etc).
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
        )),
    )(input)
}

/// Parse a backslash, followed by any amount of whitespace. This is used later
/// to discard any escaped whitespace.
fn parse_escaped_whitespace<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, &'a str, E> {
    preceded(char('\\'), multispace1)(input)
}

/// Parse a non-empty block of text that doesn't include \ or "
fn parse_literal_block<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    let not_quote_slash = is_not("\"\\");

    // we want to ensure that the output of is_not is non-empty.
    verify(not_quote_slash, |s: &str| !s.is_empty())(input)
}

/// A string fragment contains a fragment of a string being parsed: either
/// a non-empty Literal (a series of non-escaped characters), a single
/// parsed escaped character, or a block of escaped whitespace.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
    Literal(&'a str),
    EscapedChar(char),
    EscapedWS,
}

/// Combine parse_literal, parse_escaped_whitespace, and parse_escaped_char
/// into a StringFragment.
fn parse_fragment<'a, E>(input: &'a str) -> IResult<&'a str, StringFragment<'a>, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    alt((
        map(parse_literal_block, StringFragment::Literal),
        map(parse_escaped_char, StringFragment::EscapedChar),
        value(StringFragment::EscapedWS, parse_escaped_whitespace),
    ))(input)
}

/// Parse a string. Use a loop of parse_fragment and push all of the fragments
/// into an output string.
fn parse_string<'a, E>(input: &'a str) -> IResult<&'a str, String, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    let build_string = fold_many0(
        // Our parser function– parses a single string fragment
        parse_fragment,
        // Our init value, an empty string
        String::new,
        // Our folding function. For each fragment, append the fragment to the
        // string.
        |mut string, fragment| {
            match fragment {
                StringFragment::Literal(s) => string.push_str(s),
                StringFragment::EscapedChar(c) => string.push(c),
                StringFragment::EscapedWS => {}
            }
            string
        },
    );

    delimited(char('"'), build_string, char('"'))(input)
}

#[test]
fn test_parse_string() {
    let data = "\"hello world\"";
    assert_eq!(
        parse_string::<()>(data),
        Ok(("", String::from("hello world"),))
    );
    let data = "\"tab:\\tafter tab, newline:\\nnew line, quote: \\\", emoji: \\u{1F602}, newline:\\nescaped whitespace: \\    abc\"";
    assert_eq!(parse_string::<()>(data), Ok((
        "",
        String::from("tab:\tafter tab, newline:\nnew line, quote: \", emoji: 😂, newline:\nescaped whitespace: abc"),
    )));
}

/// Parses a decimal number.
fn decimal<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}

// Parses a floating point number.
fn float<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    alt((
        // Case one: .42
        recognize(tuple((
            char('.'),
            decimal,
            opt(tuple((one_of("eE"), opt(one_of("+-")), decimal))),
        ))), // Case two: 42e42 and 42.42e42
        recognize(tuple((
            decimal,
            opt(preceded(char('.'), decimal)),
            one_of("eE"),
            opt(one_of("+-")),
            decimal,
        ))), // Case three: 42. and 42.42
        recognize(tuple((decimal, char('.'), opt(decimal)))),
    ))(input)
}
