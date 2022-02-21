mod combinator;
mod string;
#[cfg(test)]
mod tests;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, none_of},
    combinator::map,
    error::ParseError,
    multi::{many1, separated_list0},
    sequence::terminated,
    sequence::{delimited, tuple},
    IResult,
};

use crate::ast::{Exp, Id, Literal, Pat, Type};

use self::{
    combinator::{decimal, float, identifier, separated_list_with, ws},
    string::{parse_escaped_char, parse_string},
};

pub fn parse<'a>(input: &'a str) -> IResult<&'a str, Exp> {
    parse_exp(input)
}

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

fn parse_ident<'a>(input: &'a str) -> IResult<&'a str, Exp> {
    ws(map(identifier, |id| {
        Exp::Ident(Id {
            name: id.to_string(),
        })
    }))(input)
}

fn parse_literal<'a>(input: &'a str) -> IResult<&'a str, Literal> {
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
