use std::{collections::HashMap, io::BufRead, io::BufReader};

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
        println!("stack: {:?}", vm.stack);
    }
}

fn parse_word(word: &str, vm: &mut Vm) {
    if word.is_empty() {
        return;
    }
    if word == "{" {
        vm.blocks.push(vec![]);
    } else if word == "}" {
        let block = vm.blocks.pop().unwrap();
        eval(Value::Block(block), vm);
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

#[derive(Clone)]
struct Vm {
    stack: Vec<Value>,
    vars: Vec<HashMap<String, Value>>,
    blocks: Vec<Vec<Value>>,
}

impl Vm {
    fn new() -> Self {
        let functions: [(&str, fn(&mut Vm)); 10] = [
            ("+", add),
            ("-", sub),
            ("*", mul),
            ("/", div),
            ("<", lt),
            ("if", op_if),
            ("def", op_def),
            ("puts", op_puts),
            //("pop", pop),
            ("dup", op_dup),
            ("exch", op_exch),
            //("index", index),
        ];
        Self {
            stack: vec![],
            vars: vec![functions
                .into_iter()
                .map(|(name, op)| (name.to_owned(), Value::Native(NativeOp(op))))
                .collect()],
            blocks: vec![],
        }
    }
    fn find_var(&self, name: &str) -> Option<Value> {
        self.vars.iter().rev().find_map(|vars| {
            let a = vars.get(name);
            //a.unwrap().to_owned()
            a.map(|var| var.to_owned())
        })
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
enum Value {
    Num(i32),
    Op(String),
    Sym(String),
    Block(Vec<Value>),
    Native(NativeOp),
}

#[derive(Clone)]
struct NativeOp(fn(&mut Vm));

impl PartialEq for NativeOp {
    fn eq(&self, other: &NativeOp) -> bool {
        self.0 == other.0
    }
}

impl Eq for NativeOp {}

impl std::fmt::Debug for NativeOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<NativeOp>")
    }
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
            Self::Sym(val) => val,
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
            Self::Num(val) => val.to_string(),
            Self::Sym(val) => val.clone(),
            Self::Block(val) => format!("{:?}", val),
            Self::Op(val) => val.clone(),
            Self::Native(val) => format!("{:?}", val),
        }
    }
}

fn eval(code: Value, vm: &mut Vm) {
    //println!("stack: {:?}", vm.stack);
    if let Some(top_block) = vm.blocks.last_mut() {
        top_block.push(code);
        return;
    }
    if let Value::Op(ref op) = code {
        let val = vm
            .find_var(op)
            //.vars
            //.get(op)
            .expect(&format!("{op:?} is not a defined operation"))
            .clone();
        match val {
            Value::Block(block) => {
                vm.vars.push(HashMap::new());
                for code in block {
                    eval(code, vm);
                }
                vm.vars.pop();
            }
            Value::Native(op) => op.0(vm),
            _ => vm.stack.push(val.clone()),
        }
    } else {
        vm.stack.push(code);
    }
}

macro_rules! impl_op{
    {$name:ident, $op: tt}=> {
        fn $name(vm: &mut Vm) {
            let rhs = vm.stack.pop().unwrap().as_num();
            let lhs = vm.stack.pop().unwrap().as_num();
            vm.stack.push(Value::Num((lhs $op rhs) as i32))
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
    vm.vars.last_mut().unwrap().insert(name, num);
}

fn op_puts(vm: &mut Vm) {
    let val = vm.stack.pop().unwrap();
    println!("{}", val.to_string());
    vm.stack.push(val);
    //println!("stack: {:?}", vm.stack);
    //println!("vars: {:?}", vm.vars);
}

fn op_dup(vm: &mut Vm) {
    let val = vm.stack.last().unwrap().clone();
    vm.stack.push(val);
}

fn op_exch(vm: &mut Vm) {
    let val1 = vm.stack.pop().unwrap();
    let val2 = vm.stack.pop().unwrap();
    vm.stack.push(val1);
    vm.stack.push(val2);
}

#[cfg(test)]
mod test {
    use super::{parse_batch, Value::*};
    use std::io::Cursor;
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

    #[test]
    fn test_multiline() {
        let multiline_input = "
        /x 1 def
        /y 2 def
        x y +
        ";
        assert_eq!(parse_batch(Cursor::new(multiline_input)), vec![Num(3)]);
    }
}
