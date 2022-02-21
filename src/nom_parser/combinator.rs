use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until},
    character::complete::{alpha1, alphanumeric1, char, multispace0, one_of},
    combinator::{map, opt, recognize, value},
    error::ParseError,
    multi::{many0, many1, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult, Parser,
};

/// Parses a identifier that may start with a letter or an underscore and may contain underscores, letters and numbers.
pub fn identifier<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

/// Parses a decimal number.
pub fn decimal<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}

// Parses a floating point number.
pub fn float<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
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

pub fn separated_list_with<'a, O, O2, E, F1, G1, F2, G2>(
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

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws<'a, O, E, F>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
    E: ParseError<&'a str>,
{
    delimited(
        alt((peol_comment, pinline_comment, map(multispace0, |_| ()))),
        inner,
        alt((peol_comment, pinline_comment, map(multispace0, |_| ()))),
    )
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
