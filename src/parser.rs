mod combinator;
mod string;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while_m_n},
    character::complete::{alpha1, alphanumeric1, char, multispace1, none_of, one_of},
    combinator::{map, map_opt, map_res, opt, recognize, value, verify},
    error::{FromExternalError, ParseError},
    multi::{fold_many0, many0, many1, separated_list0},
    sequence::{delimited, tuple},
    sequence::{pair, preceded, terminated},
    IResult,
};

use crate::ast::{Exp, Id, Literal, Pat, Type};

use self::{
    combinator::{decimal, float, identifier, separated_list_with, ws},
    string::{parse_escaped_char, parse_string},
};

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
