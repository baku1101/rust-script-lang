fn main(){
    for line in std::io::stdin().lines(){
        if let Ok(line) = line {
            let input: Vec<_> = line.split(" ").collect();
            let mut words = &input[..];
            parse_words(words);
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
enum Value<'src> {
    Num(i32),
    Op(&'src str),
    Block(Vec<Value<'src>>)
}

impl<'src> Value<'src> {
    fn as_num(&self) ->i32 {
        match self {
            Self::Num(val) => *val,
            _ => panic!("value is not a number"),
        }
    }
}

fn parse_block<'src, 'a>(
        input: &'a [&'src str],
    ) -> (Value<'src>, &'a[&'src str]) {
    let mut tokens = vec![];
    let mut words = input;
    
    while let Some((&word ,mut rest)) = words.split_first() {
        if word.is_empty() {
            break;
        }
        if word == "{" {
            let value;
            (value, rest) = parse_block(rest);
            tokens.push(value);
        } else if word == "}" {
            return (Value::Block(tokens),rest);
        } else if let Ok(value) = word.parse::<i32>() {
            tokens.push(Value::Num(value));
        } else {
            tokens.push(Value::Op(word));
        }
        words = rest;
    }
    
    (Value::Block(tokens), words)
}


fn parse_words<'src, 'a> (mut words:&'a [&'src str])-> Vec<Value<'src>>{
    let mut stack = vec![];
    while let Some((&word, mut rest)) = words.split_first(){
        if word == "{" {
            let value;
            (value, rest) = parse_block(rest);
            stack.push(value);
        }
        else {
            if let Ok(parsed) = word.parse::<i32>(){
                stack.push(Value::Num(parsed));
            } else {
                match word {
                    "+" => add(&mut stack),
                    "-" => sub(&mut stack),
                    "*" => mul(&mut stack),
                    "/" => div(&mut stack),
                    _ => panic!("{word:?} could not be parsed",),
                }
            }
        }
        words = rest;
    }
    println!("stack: {stack:?}");
    return stack;
}



fn add(stack: &mut Vec<Value>) {
    let lhs = stack.pop().unwrap().as_num();
    let rhs = stack.pop().unwrap().as_num();
    stack.push(Value::Num(rhs+lhs));
}

fn mul(stack: &mut Vec<Value>) {
    let lhs = stack.pop().unwrap().as_num();
    let rhs = stack.pop().unwrap().as_num();
    stack.push(Value::Num(rhs*lhs));
}

fn sub(stack: &mut Vec<Value>) {
    let lhs = stack.pop().unwrap().as_num();
    let rhs = stack.pop().unwrap().as_num();
    stack.push(Value::Num(rhs-lhs));
}

fn div(stack: &mut Vec<Value>) {
    let lhs = stack.pop().unwrap().as_num();
    let rhs = stack.pop().unwrap().as_num();
    stack.push(Value::Num(rhs/lhs));
}
 
#[cfg(test)]
mod test {
    use super::{parse_words, Value::*};
    #[test]
    fn test_group(){
        let test_string : Vec<_> = "1 2 + { 3 4 }".split(" ").collect();
        let test_slice : &[&str] = &test_string[..];
        assert_eq!(
            parse_words(test_slice),
            vec![Num(3), Block(vec![Num(3), Num(4)])]
            );
    }
}
