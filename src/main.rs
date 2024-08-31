use std::collections::HashMap;
use std::io::{BufRead, BufReader};
fn main() {
    if let Some(f) = std::env::args()
        .nth(1)
        .and_then(|f| std::fs::File::open(f).ok())
    {
        parse_batch(BufReader::new(f));
    } else {
        parse_interactive();
    }
}

fn parse_batch(source: impl BufRead) -> Vec<Value> {
    let mut vm = Vm::new();
    for line in source.lines().flatten() {
        for word in line.split(" ") {
            parse_word(word, &mut vm);
        }
    }
    vm.stack
}

fn parse_interactive() {
    let mut vm = Vm::new();
    for line in std::io::stdin().lines().flatten() {
        for word in line.split(" ") {
            parse_word(word, &mut vm);
        }
        println!("stack: {:?}", vm.stack)
    }
}

struct Vm {
    stack: Vec<Value>,
    vars: HashMap<String, Value>,
    blocks: Vec<Vec<Value>>,
}

impl Vm {
    fn new() -> Self {
        Self {
            stack: vec![],
            vars: HashMap::new(),
            blocks: vec![],
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
enum Value {
    Num(i32),
    Op(String),
    Sym(String),
    Block(Vec<Value>),
}

impl Value {
    fn as_num(self) -> i32 {
        match self {
            Self::Num(val) => val,
            _ => panic!("value is not a number"),
        }
    }

    fn as_sym(self) -> String {
        match self {
            Self::Sym(val) => val.clone(),
            _ => panic!("value is not a symbol"),
        }
    }

    fn to_block(self) -> Vec<Value> {
        match self {
            Self::Block(val) => val,
            _ => panic!("value is not block"),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Self::Num(i) => i.to_string(),
            Self::Op(ref s) | Self::Sym(ref s) => s.clone(),
            Self::Block(_) => "<Block>".to_string(),
        }
    }
}

fn puts(vm: &mut Vm) {
    let value = vm.stack.pop().unwrap();
    println!("{}", value.to_string());
}

fn parse_word(word: &str, vm: &mut Vm) {
    if word.is_empty() {
        return;
    }
    if word == "{" {
        vm.blocks.push(vec![]);
    } else if word == "}" {
        let top_block = vm.blocks.pop().expect("Block stack underrun");
        eval(Value::Block(top_block), vm);
    } else {
        let code = if let Ok(parsed) = word.parse::<i32>() {
            Value::Num(parsed)
        } else if word.starts_with("/") {
            Value::Sym(word[1..].to_string())
        } else {
            Value::Op(word.to_string())
        };
        eval(code, vm);
    }
}

fn eval(code: Value, vm: &mut Vm) {
    if let Some(top_block) = vm.blocks.last_mut() {
        top_block.push(code);
        return;
    }
    match code {
        Value::Op(ref op) => match op as &str {
            "+" => add(&mut vm.stack),
            "-" => sub(&mut vm.stack),
            "*" => mul(&mut vm.stack),
            "/" => div(&mut vm.stack),
            "<" => lt(&mut vm.stack),
            "if" => op_if(vm),
            "puts" => puts(vm),
            "def" => op_def(vm),
            _ => {
                let val = vm
                    .vars
                    .get(op)
                    .expect(&format!("{op:?} is not a defined operation"));
                vm.stack.push(val.clone());
            }
        },
        _ => vm.stack.push(code.clone()),
    }
}

macro_rules! impl_op{
    {$name:ident, $op: tt}=> {
        fn $name(stack: &mut Vec<Value>) {
            let rhs = stack.pop().unwrap().as_num();
            let lhs = stack.pop().unwrap().as_num();
            stack.push(Value::Num((lhs $op rhs) as i32))
        }
    }
}

impl_op!(add, +);
impl_op!(sub, -);
impl_op!(mul, *);
impl_op!(div, /);
impl_op!(lt, <);

fn op_if(vm: &mut Vm) {
    let false_branch = vm.stack.pop().unwrap().to_block();
    let true_branch = vm.stack.pop().unwrap().to_block();
    let cond = vm.stack.pop().unwrap().to_block();

    for code in cond {
        eval(code, vm);
    }

    let cond_result = vm.stack.pop().unwrap().as_num();

    if cond_result != 0 {
        for code in true_branch {
            eval(code, vm);
        }
    } else {
        for code in false_branch {
            eval(code, vm);
        }
    }
}

fn op_def(vm: &mut Vm) {
    let num = vm.stack.pop().unwrap();
    let name = vm.stack.pop().unwrap().as_sym();
    vm.vars.insert(name, num);
}

#[cfg(test)]
mod test {
    use std::io::Cursor;

    use super::{parse_batch, Value::*};
    #[test]
    fn test_group() {
        assert_eq!(
            parse_batch(Cursor::new("1 2 + { 3 4 }")),
            vec![Num(3), Block(vec![Num(3), Num(4)])]
        );
    }
    #[test]
    fn test_def() {
        assert_eq!(parse_batch(Cursor::new("/x 1 def x")), vec![Num(1)]);
    }
}
