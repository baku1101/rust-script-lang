fn main() {
    let input = "2 + 3 + 4";
    println!("expr: {:?}", expr(input));
    let input = "2+3+4 + ( 5 + 6 )";
    println!("expr: {:?}", expr(input));
}

#[derive(Debug, PartialEq)]
enum Expression<'src> {
    Indent(&'src str),
    NumLiteral(f64),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
}

fn expr(input: &str) -> Option<(&str, Expression)> {
    if let Some(res) = add(input) {
        return Some(res);
    }
    if let Some(res) = term(input) {
        return Some(res);
    }
    None
}

fn add(mut input: &str) -> Option<(&str, Expression)> {
    let mut left = None;
    while let Some((next_input, expr)) = add_term(input) {
        if let Some(prev_left) = left {
            left = Some(Expression::Add(Box::new(prev_left), Box::new(expr)))
        } else {
            left = Some(expr)
        }
        input = next_input;
    }
    let left = left?;
    let (next_input, rhs) = expr(input)?;
    Some((next_input, Expression::Add(Box::new(left), Box::new(rhs))))
}

fn add_term(input: &str) -> Option<(&str, Expression)> {
    let (next_input, lhs) = term(input)?;
    let (next_input, _) = plus(whitespace(next_input))?;
    Some((next_input, lhs))
}

fn term(input: &str) -> Option<(&str, Expression)> {
    if let Some(res) = paren(input) {
        return Some(res);
    }
    if let Some(res) = token(input) {
        return Some(res);
    }
    None
}

fn paren(input: &str) -> Option<(&str, Expression)> {
    let next_input = lparen(whitespace(input))?;
    let (next_input, expr) = expr(next_input)?;
    let next_input = rparen(whitespace(next_input))?;
    Some((next_input, expr))
}

fn token(i: &str) -> Option<(&str, Expression)> {
    if let Some(res) = ident(whitespace(i)) {
        return Some(res);
    }
    if let Some(res) = number(whitespace(i)) {
        return Some(res);
    }
    None
}

fn whitespace(mut input: &str) -> &str {
    while matches!(peek_char(input), Some(' ')) {
        input = advance_char(input);
    }
    input
}

fn plus(mut input: &str) -> Option<(&str, Expression)> {
    if matches!(peek_char(input), Some('+')) {
        input = advance_char(input);
        Some((input, Expression::Indent("+")))
    } else {
        None
    }
}

fn number(mut input: &str) -> Option<(&str, Expression)> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('-' | '+' | '0'..='9'))) {
        input = advance_char(input);
        while matches!(peek_char(input), Some(_x @ ('.' | '0'..='9'))) {
            input = advance_char(input);
        }
        if let Ok(num) = start[..(start.len() - input.len())].parse::<f64>() {
            Some((input, Expression::NumLiteral(num)))
        } else {
            None
        }
    } else {
        None
    }
}

fn ident(mut input: &str) -> Option<(&str, Expression)> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('a'..='z' | 'A'..='Z'))) {
        while matches!(
            peek_char(input),
            Some(_x @ ('a'..='z' | 'A'..='Z' | '0'..='9'))
        ) {
            input = advance_char(input);
        }
        Some((
            input,
            Expression::Indent(&start[..(start.len() - input.len())]),
        ))
    } else {
        None
    }
}

fn lparen(mut input: &str) -> Option<&str> {
    if matches!(peek_char(input), Some('(')) {
        input = advance_char(input);
        Some(input)
    } else {
        None
    }
}

fn rparen(mut input: &str) -> Option<&str> {
    if matches!(peek_char(input), Some(')')) {
        input = advance_char(input);
        Some(input)
    } else {
        None
    }
}

fn advance_char(input: &str) -> &str {
    let mut chars = input.chars();
    chars.next();
    chars.as_str()
}

fn peek_char(input: &str) -> Option<char> {
    input.chars().next()
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_whitespace() {
//         assert_eq!(whitespace("  "), "");
//         assert_eq!(whitespace("  a"), "a");
//         assert_eq!(whitespace("a"), "a");
//     }

//     #[test]
//     fn test_number() {
//         assert_eq!(number("123"), "");
//         assert_eq!(number("123.456"), "");
//         assert_eq!(number("123.456a"), "a");
//         assert_eq!(number("a"), "a");
//         assert_eq!(number("-123"), "");
//     }

//     #[test]
//     fn test_ident() {
//         assert_eq!(ident("abc"), "");
//         assert_eq!(ident("abc123"), "");
//         assert_eq!(ident("abc123a"), "");
//         assert_eq!(ident("123"), "123");
//     }
// }
