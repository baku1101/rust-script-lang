fn main() {
    let input = "(123 (tree)world)";
    println!("source: {:?}", source(input));
}

#[derive(Debug, PartialEq)]
enum Token<'src> {
    Indent(&'src str),
    Number(f64),
    LParen,
    RParen,
}

#[derive(Debug, PartialEq)]
enum TokenTree<'src> {
    Token(Token<'src>),
    Tree(Vec<TokenTree<'src>>),
}

fn source(mut input: &str) -> (&str, TokenTree) {
    let mut tokens = vec![];
    while !input.is_empty() {
        input = if let Some((next_input, token)) = token(input) {
            match token {
                Token::LParen => {
                    let (next_input, tt) = source(next_input);
                    tokens.push(tt);
                    next_input
                }
                Token::RParen => {
                    return (next_input, TokenTree::Tree(tokens));
                }
                _ => {
                    tokens.push(TokenTree::Token(token));
                    next_input
                }
            }
        } else {
            break;
        };
    }
    (input, TokenTree::Tree(tokens))
}

fn token(i: &str) -> Option<(&str, Token)> {
    if let Some(res) = ident(whitespace(i)) {
        return Some(res);
    }
    if let Some(res) = number(whitespace(i)) {
        return Some(res);
    }
    if let Some(res) = lparen(whitespace(i)) {
        return Some(res);
    }
    if let Some(res) = rparen(whitespace(i)) {
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

fn number(mut input: &str) -> Option<(&str, Token)> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('-' | '+' | '0'..='9'))) {
        if matches!(peek_char(input), Some(_x @ ('-' | '+'))) {
            input = advance_char(input);
        }
        while matches!(peek_char(input), Some(_x @ ('.' | '0'..='9'))) {
            input = advance_char(input);
        }
        if let Ok(num) = start[..(start.len() - input.len())].parse::<f64>() {
            Some((input, Token::Number(num)))
        } else {
            None
        }
    } else {
        None
    }
}

fn ident(mut input: &str) -> Option<(&str, Token)> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('a'..='z' | 'A'..='Z'))) {
        while matches!(
            peek_char(input),
            Some(_x @ ('a'..='z' | 'A'..='Z' | '0'..='9'))
        ) {
            input = advance_char(input);
        }
        Some((input, Token::Indent(&start[..(start.len() - input.len())])))
    } else {
        None
    }
}

fn lparen(mut input: &str) -> Option<(&str, Token)> {
    if matches!(peek_char(input), Some('(')) {
        input = advance_char(input);
        Some((input, Token::LParen))
    } else {
        None
    }
}

fn rparen(mut input: &str) -> Option<(&str, Token)> {
    if matches!(peek_char(input), Some(')')) {
        input = advance_char(input);
        Some((input, Token::RParen))
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
