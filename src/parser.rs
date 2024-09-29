use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0},
    combinator::recognize,
    error::ParseError,
    multi::{fold_many0, many0},
    number::complete::recognize_float,
    sequence::{delimited, pair},
    IResult, Parser,
};

fn main() {
    fn ex_eval<'src>(input: &'src str) -> Result<f64, nom::Err<nom::error::Error<&'src str>>> {
        expr(input).map(|(_, e)| eval(e))
    }
    let input = "123";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));

    let input = "(123 + 456 ) + pi";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));

    let input = "10 + (100 - 1)";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));

    let input = "((1 + 2) + (3 + 4)) + 5 + 6";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));

    let input = "1 + 2 * 3";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));
}

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
}

fn eval(expr: Expression) -> f64 {
    match expr {
        Expression::Ident("pi") => std::f64::consts::PI,
        Expression::Ident(id) => panic!("Unknown name {:?}", id),
        Expression::NumLiteral(n) => n,
        Expression::Add(lhs, rhs) => eval(*lhs) + eval(*rhs),
        Expression::Sub(lhs, rhs) => eval(*lhs) - eval(*rhs),
        Expression::Mul(lhs, rhs) => eval(*lhs) * eval(*rhs),
        Expression::Div(lhs, rhs) => eval(*lhs) / eval(*rhs),
    }
}

fn expr(input: &str) -> IResult<&str, Expression> {
    let (i, init) = term(input)?;

    fold_many0(
        pair(space_delimited(alt((char('+'), char('-')))), term),
        move || init.clone(),
        |acc, (_op, val): (char, Expression)| match _op {
            '+' => Expression::Add(Box::new(acc), Box::new(val)),
            '-' => Expression::Sub(Box::new(acc), Box::new(val)),
            _ => panic!(
                "sub or add expression should be here, '+' or '-' but got {:?}",
                _op
            ),
        },
    )(i)
}

fn term(input: &str) -> IResult<&str, Expression> {
    let (i, init) = factor(input)?;

    fold_many0(
        pair(space_delimited(alt((char('*'), char('/')))), factor),
        move || init.clone(),
        |acc, (_op, val): (char, Expression)| match _op {
            '*' => Expression::Mul(Box::new(acc), Box::new(val)),
            '/' => Expression::Div(Box::new(acc), Box::new(val)),
            _ => panic!(
                "Multiplicative Expression should be here, '*' or '/' but got {:?}",
                _op
            ),
        },
    )(i)
}

fn factor(input: &str) -> IResult<&str, Expression> {
    alt((number, ident, parens))(input)
}

fn parens(input: &str) -> IResult<&str, Expression> {
    delimited(tag("("), expr, tag(")"))(input)
}

fn number(input: &str) -> IResult<&str, Expression> {
    let (r, v) = space_delimited(recognize_float)(input)?;
    Ok((
        r,
        Expression::NumLiteral(v.parse().map_err(|_| {
            nom::Err::Error(nom::error::Error {
                input,
                code: nom::error::ErrorKind::Digit,
            })
        })?),
    ))
}

fn ident(input: &str) -> IResult<&str, Expression> {
    let (r, v) = space_delimited(identifier)(input)?;
    Ok((r, Expression::Ident(v)))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn space_delimited<'src, O, E>(
    f: impl Parser<&'src str, O, E>,
) -> impl FnMut(&'src str) -> IResult<&'src str, O, E>
where
    E: ParseError<&'src str>,
{
    delimited(multispace0, f, multispace0)
}
