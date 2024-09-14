fn whitespace(mut input: &str) -> &str {
    while matches!(input.chars().next(), Some(' ')) {
        let mut chars = input.chars();
        chars.next();
        input = chars.as_str();
    }
    input
}

fn number(mut input: &str) -> &str {
    if matches!(input.chars().next(), Some(_x @ ('-' | '+' | '0'..='9'))) {
        if matches!(input.chars().next(), Some(_x @ ('-' | '+'))) {
            let mut chars = input.chars();
            chars.next();
            input = chars.as_str();
        }
        while matches!(input.chars().next(), Some(_x @ ('.' | '0'..='9'))) {
            let mut chars = input.chars();
            chars.next();
            input = chars.as_str();
        }
    }
    input
}

fn ident(mut input: &str) -> &str {
    if matches!(input.chars().next(), Some(_x @ ('a'..='z' | 'A'..='Z'))) {
        while matches!(
            input.chars().next(),
            Some(_x @ ('a'..='z' | 'A'..='Z' | '0'..='9'))
        ) {
            let mut chars = input.chars();
            chars.next();
            input = chars.as_str();
        }
    }
    input
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_whitespace() {
        assert_eq!(whitespace("  "), "");
        assert_eq!(whitespace("  a"), "a");
        assert_eq!(whitespace("a"), "a");
    }

    #[test]
    fn test_number() {
        assert_eq!(number("123"), "");
        assert_eq!(number("123.456"), "");
        assert_eq!(number("123.456a"), "a");
        assert_eq!(number("a"), "a");
        assert_eq!(number("-123"), "");
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident("abc"), "");
        assert_eq!(ident("abc123"), "");
        assert_eq!(ident("abc123a"), "");
        assert_eq!(ident("123"), "123");
    }
}
