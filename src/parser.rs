#[derive(Debug, PartialEq)]
enum Expression<'src> {
    Ident(&'src str),
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
    if let Some((i, ident_res)) = ident(whitespace(i)) {
        return Some((i, ident_res));
    }
    if let Some((i, number_res)) = number(whitespace(i)) {
        return Some((i, number_res));
    }
    None
}

fn add(mut input: &str) -> Option<(&str, Expression)> {
    let mut left = None;
    while let Some((next_input, expr)) = add_term(input) {
        if let Some(prev_left) = left {
            left = Some(Expression::Add(Box::new(prev_left), Box::new(expr)));
        } else {
            left = Some(expr);
        }
        input = next_input;
    }
    let (input, last) = term(input)?;
    if let Some(left) = left {
        return Some((input, Expression::Add(Box::new(left), Box::new(last))));
    } else {
        None
    }
}

fn add_term(input: &str) -> Option<(&str, Expression)> {
    let (next_input, lhs) = term(input)?;
    let (next_input, _) = plus(whitespace(next_input))?;
    Some((next_input, lhs))
}

fn plus(input: &str) -> Option<(&str, Expression)> {
    if matches!(peek_char(input), Some('+')) {
        Some((advance_char(input), Expression::Ident("+")))
    } else {
        None
    }
}

fn whitespace(mut input: &str) -> &str {
    while matches!(input.chars().next(), Some(' ')) {
        input = advance_char(input);
    }
    input
}

fn number(mut input: &str) -> Option<(&str, Expression)> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('-' | '+' | '.' | '0'..='9'))) {
        input = advance_char(input);
        while matches!(peek_char(input), Some(_x @ ('.' | '0'..='9'))) {
            input = advance_char(input);
        }
        if let Some(num) = start[..start.len() - input.len()].parse().ok() {
            return Some((input, Expression::NumLiteral(num)));
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
        input = advance_char(input);
        while matches!(
            peek_char(input),
            Some(_x @ ('a'..='z' | 'A'..='Z' | '0'..='9'))
        ) {
            input = advance_char(input);
        }
        Some((
            input,
            Expression::Ident(&start[..start.len() - input.len()]),
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

fn main() {
    let input = "(1 + 3)+(5+6)";
    println!("source: {:?}, parsed: {:?}", input, expr(input));
    let input = "12 + 3 +4+5";
    println!("source: {:?}, parsed: {:?}", input, expr(input));
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_whtitespace() {
        assert_eq!(whitespace("  hello"), "hello");
    }

    #[test]
    fn test_number() {
        assert_eq!(number("+123 "), (" ", Some(Token::Number)));
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident("hello00"), ("", Some(Token::Ident)));
    }
}
