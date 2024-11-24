use crate::ast::{ExprEnum, Expression, Statement, Statements, TypeDecl};
use crate::typechecker::Span;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1, none_of},
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, preceded, terminated},
    IResult, InputTake, Offset, Parser,
};

fn space_delimited<'src, O, E>(
    f: impl Parser<Span<'src>, O, E>,
) -> impl FnMut(Span<'src>) -> IResult<Span<'src>, O, E>
where
    E: ParseError<Span<'src>>,
{
    delimited(multispace0, f, multispace0)
}

pub fn statements(i: Span) -> IResult<Span, Statements> {
    let (i, stmts) = many0(statement)(i)?;
    Ok((i, stmts))
}

fn statement(i: Span) -> IResult<Span, Statement> {
    alt((
        var_def,
        var_assign,
        fn_def_statement,
        for_statement,
        terminated(return_statement, char(';')),
        terminated(break_statement, char(';')),
        terminated(continue_statement, char(';')),
        terminated(expr_statement, char(';')),
    ))(i)
}

fn break_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("break"))(i)?;
    Ok((i, Statement::Break))
}

fn continue_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("continue"))(i)?;
    Ok((i, Statement::Continue))
}

fn return_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("return"))(i)?;
    let (i, expr) = expr(i)?;
    Ok((i, Statement::Return(expr)))
}

fn fn_def_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("fn"))(i)?;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("("))(i)?;
    let (i, args) = separated_list0(char(','), space_delimited(argument))(i)?;
    let (i, _) = space_delimited(tag(")"))(i)?;
    let (i, _) = space_delimited(tag("->"))(i)?;
    let (i, ret_type) = type_decl(i)?;
    let (i, stmts) = delimited(open_brace, statements, close_brace)(i)?;
    Ok((
        i,
        Statement::FnDef {
            name,
            args,
            ret_type,
            stmts,
        },
    ))
}

fn for_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("for"))(i)?;
    let (i, loop_var) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("in"))(i)?;
    let (i, start) = space_delimited(expr)(i)?;
    let (i, _) = space_delimited(tag("to"))(i)?;
    let (i, end) = space_delimited(expr)(i)?;
    let (i, stmts) = delimited(open_brace, statements, close_brace)(i)?;
    Ok((
        i,
        Statement::For {
            span: i,
            loop_var,
            start,
            end,
            stmts,
        },
    ))
}

fn type_decl(i: Span) -> IResult<Span, TypeDecl> {
    let (i, td) = space_delimited(identifier)(i)?;
    Ok((
        i,
        match td.into_fragment() {
            "i64" => TypeDecl::I64,
            "f64" => TypeDecl::F64,
            "str" => TypeDecl::Str,
            _ => {
                panic!("Type annotation has unknown type: {td}")
            }
        },
    ))
}

fn argument(i: Span) -> IResult<Span, (Span, TypeDecl)> {
    let (i, ident) = space_delimited(identifier)(i)?;
    let (i, _) = char(':')(i)?;
    let (i, td) = type_decl(i)?;

    Ok((i, (ident, td)))
}

fn var_def(i: Span) -> IResult<Span, Statement> {
    let span = i;
    let (i, _) = delimited(multispace0, tag("var"), multispace1)(i)?;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(char(':'))(i)?;
    let (i, td) = type_decl(i)?;
    let (i, _) = space_delimited(char('='))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    let (i, _) = space_delimited(char(';'))(i)?;
    Ok((
        i,
        Statement::VarDef {
            span: calc_offset(span, i),
            name,
            td,
            expr,
        },
    ))
}

fn var_assign(i: Span) -> IResult<Span, Statement> {
    let span = i;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("="))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    let (i, _) = space_delimited(tag(";"))(i)?;
    Ok((
        i,
        Statement::VarAssign {
            span: calc_offset(span, i),
            name,
            expr,
        },
    ))
}

fn expr_statement(i: Span) -> IResult<Span, Statement> {
    let (i, expr) = expr(i)?;
    Ok((i, Statement::Expression(expr)))
}

fn expr(input: Span) -> IResult<Span, Expression> {
    alt((if_expr, cond_expr, num_expr))(input)
}

fn if_expr(i: Span) -> IResult<Span, Expression> {
    let i0 = i;
    let (i, _) = space_delimited(tag("if"))(i)?;
    let (i, cond) = space_delimited(expr)(i)?;
    let (i, then) = delimited(open_brace, statements, close_brace)(i)?;
    let (i, els) = opt(preceded(
        space_delimited(tag("else")),
        delimited(open_brace, statements, close_brace),
    ))(i)?;
    Ok((
        i,
        Expression::new(
            ExprEnum::If(Box::new(cond), Box::new(then), els.map(Box::new)),
            calc_offset(i0, i),
        ),
    ))
}

fn open_brace(i: Span) -> IResult<Span, ()> {
    let (i, _) = space_delimited(tag("{"))(i)?;
    Ok((i, ()))
}

fn close_brace(i: Span) -> IResult<Span, ()> {
    let (i, _) = space_delimited(tag("}"))(i)?;
    Ok((i, ()))
}

fn cond_expr(i: Span) -> IResult<Span, Expression> {
    let i0 = i;
    let (i, lhs) = space_delimited(term)(i)?;
    let (i, op) = alt((
        space_delimited(tag("==")),
        space_delimited(tag("!=")),
        space_delimited(tag(">=")),
        space_delimited(tag("<=")),
        space_delimited(tag(">")),
        space_delimited(tag("<")),
    ))(i)?;
    let (i, rhs) = space_delimited(term)(i)?;
    Ok((
        i,
        match op.into_fragment() {
            "==" => Expression::new(
                ExprEnum::Eq(Box::new(lhs), Box::new(rhs)),
                calc_offset(i0, i),
            ),
            "!=" => Expression::new(
                ExprEnum::Ne(Box::new(lhs), Box::new(rhs)),
                calc_offset(i0, i),
            ),
            ">=" => Expression::new(
                ExprEnum::Gte(Box::new(lhs), Box::new(rhs)),
                calc_offset(i0, i),
            ),
            "<=" => Expression::new(
                ExprEnum::Lte(Box::new(lhs), Box::new(rhs)),
                calc_offset(i0, i),
            ),
            ">" => Expression::new(
                ExprEnum::Gt(Box::new(lhs), Box::new(rhs)),
                calc_offset(i0, i),
            ),
            "<" => Expression::new(
                ExprEnum::Lt(Box::new(lhs), Box::new(rhs)),
                calc_offset(i0, i),
            ),
            _ => unreachable!(),
        },
    ))
}

fn num_expr(input: Span) -> IResult<Span, Expression> {
    let (i, init) = expr_muldiv(input)?;
    fold_many0(
        pair(space_delimited(alt((char('+'), char('-')))), expr_muldiv),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| {
            let span = calc_offset(input, acc.span);
            match op {
                '+' => Expression::new(ExprEnum::Add(Box::new(acc), Box::new(val)), span),
                '-' => Expression::new(ExprEnum::Sub(Box::new(acc), Box::new(val)), span),
                _ => unreachable!(),
            }
        },
    )(i)
}

fn expr_muldiv(input: Span) -> IResult<Span, Expression> {
    let (i, init) = term(input)?;

    fold_many0(
        pair(space_delimited(alt((char('*'), char('/')))), term),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| {
            let span = calc_offset(input, acc.span);
            match op {
                '*' => Expression::new(ExprEnum::Mul(Box::new(acc), Box::new(val)), span),
                '/' => Expression::new(ExprEnum::Div(Box::new(acc), Box::new(val)), span),
                _ => unreachable!(),
            }
        },
    )(i)
}

fn term(input: Span) -> IResult<Span, Expression> {
    alt((paren, func_call, token))(input)
}

fn paren(input: Span) -> IResult<Span, Expression> {
    delimited(
        multispace0,
        delimited(tag("("), expr, tag(")")),
        multispace0,
    )(input)
}

fn token(i: Span) -> IResult<Span, Expression> {
    alt((ident, number, str))(i)
}

fn number(input: Span) -> IResult<Span, Expression> {
    let (r, v) = delimited(multispace0, recognize_float, multispace0)(input)?;
    let ret_expr = ExprEnum::Number(v.parse().map_err(|_| {
        nom::Err::Error(nom::error::Error {
            input,
            code: nom::error::ErrorKind::Digit,
        })
    })?);
    Ok((r, Expression::new(ret_expr, calc_offset(input, r))))
}

fn str(i: Span) -> IResult<Span, Expression> {
    let (r, _) = preceded(multispace0, char('\"'))(i)?;
    let (r, val) = many0(none_of("\""))(r)?;
    let (r, _) = terminated(char('"'), multispace0)(r)?;
    let ret_expr = ExprEnum::Str(
        val.iter()
            .collect::<String>()
            .replace("\\\\", "\\")
            .replace("\\n", "\n"),
    );
    Ok((r, Expression::new(ret_expr, calc_offset(i, r))))
}

fn ident(input: Span) -> IResult<Span, Expression> {
    let (r, v) = delimited(multispace0, identifier, multispace0)(input)?;
    Ok((
        r,
        Expression::new(ExprEnum::Ident(v.into_fragment()), calc_offset(input, r)),
    ))
}

fn identifier(input: Span) -> IResult<Span, Span> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn func_call(input: Span) -> IResult<Span, Expression> {
    let (r, ident) = space_delimited(identifier)(input)?;
    let (r, args) = space_delimited(delimited(
        tag("("),
        many0(delimited(multispace0, expr, space_delimited(opt(tag(","))))),
        tag(")"),
    ))(r)?;
    Ok((
        r,
        Expression::new(ExprEnum::FnInvoke(ident, args), calc_offset(input, r)),
    ))
}

pub fn calc_offset<'src>(i: Span<'src>, r: Span<'src>) -> Span<'src> {
    i.take(i.offset(&r))
}
