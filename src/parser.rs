use crate::ast::{calc_offset, ExprEnum, Expression, Span, Statement, Statements, TypeDecl};
use core::panic;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0, none_of},
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, preceded, terminated},
    Finish, IResult, Parser,
};

fn var_def(i: Span) -> IResult<Span, Statement> {
    let (r, _) = space_delimited(tag("var"))(i)?;
    let (r, name) = space_delimited(identifier)(r)?;
    let (r, _) = space_delimited(char(':'))(r)?;
    let (r, td) = type_decl(r)?;
    let (r, _) = space_delimited(tag("="))(r)?;
    let (r, expr) = space_delimited(expr)(r)?;
    let (r, _) = space_delimited(tag(";"))(r)?;
    Ok((
        r,
        Statement::VarDef {
            span: calc_offset(i, r),
            name,
            td,
            expr,
        },
    ))
}

fn var_assign(i: Span) -> IResult<Span, Statement> {
    let (r, name) = space_delimited(identifier)(i)?;
    let (r, _) = space_delimited(tag("="))(r)?;
    let (r, expr) = space_delimited(expr)(r)?;
    let (r, _) = space_delimited(tag(";"))(r)?;
    Ok((
        r,
        Statement::VarAssign {
            span: calc_offset(i, r),
            name,
            expr,
        },
    ))
}

fn for_statement(i: Span) -> IResult<Span, Statement> {
    let (r, _) = space_delimited(tag("for"))(i)?;
    let (r, loop_var) = space_delimited(identifier)(r)?;
    let (r, _) = space_delimited(tag("in"))(r)?;
    let (r, start) = space_delimited(expr)(r)?;
    let (r, _) = space_delimited(tag("to"))(r)?;
    let (r, end) = space_delimited(expr)(r)?;
    let (r, stmts) = delimited(open_brace, statements, close_brace)(r)?;
    Ok((
        r,
        Statement::For {
            span: calc_offset(i, r),
            loop_var,
            start,
            end,
            stmts,
        },
    ))
}

fn argument(i: Span) -> IResult<Span, (Span, TypeDecl)> {
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(char(':'))(i)?;
    let (i, td) = type_decl(i)?;
    Ok((i, (name, td)))
}

fn fn_def_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("fn"))(i)?;
    let (i, name) = space_delimited(identifier)(i)?;
    let (i, args) = delimited(
        tag("("),
        separated_list0(tag(","), space_delimited(argument)),
        tag(")"),
    )(i)?;
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

fn return_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("return"))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    Ok((i, Statement::Return(expr)))
}

fn break_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("break"))(i)?;
    Ok((i, Statement::Break))
}

fn continue_statement(i: Span) -> IResult<Span, Statement> {
    let (i, _) = space_delimited(tag("continue"))(i)?;
    Ok((i, Statement::Continue))
}

fn expr_statement(i: Span) -> IResult<Span, Statement> {
    let (i, expr) = expr(i)?;
    Ok((i, Statement::Expression(expr)))
}

fn expr(i: Span) -> IResult<Span, Expression> {
    alt((if_expr, cond_expr, num_expr))(i)
}

fn if_expr(i: Span) -> IResult<Span, Expression> {
    let (r, _) = space_delimited(tag("if"))(i)?;
    let (r, cond) = expr(r)?;
    let (r, t_case) = delimited(open_brace, statements, close_brace)(r)?;
    let (r, f_case) = opt(preceded(
        space_delimited(tag("else")),
        delimited(open_brace, statements, close_brace),
    ))(r)?;

    Ok((
        r,
        Expression::new(
            ExprEnum::If(Box::new(cond), Box::new(t_case), f_case.map(Box::new)),
            calc_offset(i, r),
        ),
    ))
}

fn cond_expr(i: Span) -> IResult<Span, Expression> {
    let (r, lhs) = num_expr(i)?;
    let (r, op) = alt((tag(">"), tag("<")))(r)?;
    let (r, rhs) = num_expr(r)?;
    Ok((
        r,
        match op.into_fragment() {
            ">" => Expression::new(ExprEnum::Gt(Box::new(lhs), Box::new(rhs)), i),
            "<" => Expression::new(ExprEnum::Lt(Box::new(lhs), Box::new(rhs)), i),
            _ => panic!("unexpected operator: {:?}", op),
        },
    ))
}

fn num_expr(i: Span) -> IResult<Span, Expression> {
    let (r, init) = term(i)?;

    let res = fold_many0(
        pair(space_delimited(alt((char('+'), char('-')))), term),
        move || init.clone(),
        |acc, (_op, val): (char, Expression)| {
            let span = calc_offset(i, acc.span);
            match _op {
                '+' => Expression::new(ExprEnum::Add(Box::new(acc), Box::new(val)), span),
                '-' => Expression::new(ExprEnum::Sub(Box::new(acc), Box::new(val)), span),
                _ => panic!(
                    "sub or add expression should be here, '+' or '-' but got {:?}",
                    _op
                ),
            }
        },
    )(r);
    res
}

fn term(i: Span) -> IResult<Span, Expression> {
    let (r, init) = factor(i)?;

    let res = fold_many0(
        pair(space_delimited(alt((char('*'), char('/')))), factor),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| {
            let span = calc_offset(i, acc.span);
            match op {
                '*' => Expression::new(ExprEnum::Mul(Box::new(acc), Box::new(val)), span),
                '/' => Expression::new(ExprEnum::Div(Box::new(acc), Box::new(val)), span),
                _ => panic!(
                    "Multiplicative Expression should be here, '*' or '/' but got {:?}",
                    op
                ),
            }
        },
    )(r);
    res
}

fn factor(i: Span) -> IResult<Span, Expression> {
    alt((str_literal, number_literal, func_call, ident, parens))(i)
}

fn func_call(i: Span) -> IResult<Span, Expression> {
    let (r, ident) = space_delimited(identifier)(i)?;
    let (r, args) = space_delimited(delimited(
        tag("("),
        many0(delimited(multispace0, expr, opt(tag(",")))),
        tag(")"),
    ))(r)?;
    Ok((
        r,
        Expression::new(ExprEnum::FnInvoke(ident, args), calc_offset(i, r)),
    ))
}

fn type_decl(i: Span) -> IResult<Span, TypeDecl> {
    let (r, td) = space_delimited(identifier)(i)?;
    Ok((
        r,
        match *td.fragment() {
            "f64" => TypeDecl::F64,
            "i64" => TypeDecl::I64,
            "str" => TypeDecl::Str,
            _ => panic!("Unknown type declaration: {:?}", td),
        },
    ))
}

fn number_literal(i: Span) -> IResult<Span, Expression> {
    let (r, v) = space_delimited(recognize_float)(i)?;
    Ok((
        r,
        Expression::new(
            ExprEnum::NumLiteral(v.parse().map_err(|_| {
                nom::Err::Error(nom::error::Error {
                    input: i,
                    code: nom::error::ErrorKind::Digit,
                })
            })?),
            i,
        ),
    ))
}

fn str_literal(i: Span) -> IResult<Span, Expression> {
    let (r, _) = preceded(multispace0, char('\"'))(i)?;
    let (r, val) = many0(none_of("\""))(r)?;
    let (r, _) = terminated(char('"'), multispace0)(r)?;
    Ok((
        r,
        Expression::new(
            ExprEnum::StrLiteral(
                val.iter()
                    .collect::<String>()
                    .replace("\\n", "\n")
                    .replace("\\\\", "\\"),
            ),
            i,
        ),
    ))
}

fn ident(i: Span) -> IResult<Span, Expression> {
    let (r, v) = space_delimited(identifier)(i)?;
    Ok((r, Expression::new(ExprEnum::Ident(v), i)))
}

fn identifier(i: Span) -> IResult<Span, Span> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(i)
}

fn space_delimited<'src, O, E>(
    f: impl Parser<Span<'src>, O, E>,
) -> impl FnMut(Span<'src>) -> IResult<Span<'src>, O, E>
where
    E: ParseError<Span<'src>>,
{
    delimited(multispace0, f, multispace0)
}

fn open_brace(i: Span) -> IResult<Span, Span> {
    space_delimited(tag("{"))(i)
}

fn close_brace(i: Span) -> IResult<Span, Span> {
    space_delimited(tag("}"))(i)
}

fn parens(i: Span) -> IResult<Span, Expression> {
    delimited(tag("("), expr, tag(")"))(i)
}

fn general_statement<'a>(is_last: bool) -> impl Fn(Span<'a>) -> IResult<Span<'a>, Statement> {
    let terminator = move |i| -> IResult<Span, ()> {
        let mut semicolon = pair(tag(";"), multispace0);
        if is_last {
            Ok((opt(semicolon)(i)?.0, ()))
        } else {
            Ok((semicolon(i)?.0, ()))
        }
    };
    move |i| {
        alt((
            var_def,
            var_assign,
            for_statement,
            fn_def_statement,
            terminated(return_statement, terminator),
            terminated(break_statement, terminator),
            terminated(continue_statement, terminator),
            terminated(expr_statement, terminator),
        ))(i)
    }
}

fn statement(i: Span) -> IResult<Span, Statement> {
    general_statement(false)(i)
}

fn last_statement(i: Span) -> IResult<Span, Statement> {
    general_statement(true)(i)
}

fn statements(i: Span) -> IResult<Span, Statements> {
    let (i, mut stmts) = many0(statement)(i)?;
    let (i, last) = opt(last_statement)(i)?;
    let (i, _) = multispace0(i)?;
    if let Some(last) = last {
        stmts.push(last);
    };
    Ok((i, stmts))
}

pub fn statements_finish(i: Span) -> Result<Statements, nom::error::Error<Span>> {
    let (_, res) = statements(i).finish()?;
    Ok(res)
}
