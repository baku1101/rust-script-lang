use core::panic;
use rust_script_lang::{
    parser::statements_finish,
    vm::{eval_statements, StackFrame},
};

use std::io::Read;
fn main() {
    let mut buf = String::new();
    if !std::io::stdin().read_to_string(&mut buf).is_ok() {
        panic!("Failed to read stdin");
    }
    let parsed_statements = match statements_finish(&buf) {
        Ok(parsed_statements) => parsed_statements,
        Err(e) => {
            eprintln!("Parsed error: {:?}", e);
            return;
        }
    };
    let mut frame = StackFrame::new();
    eval_statements(&parsed_statements, &mut frame);
}
