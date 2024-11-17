use core::panic;
use rust_script_lang::{
    parser::statements_finish,
    type_checker::{type_check, TypeCheckContext},
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

    let mut tc_ctx = TypeCheckContext::new();
    if let Err(err) = type_check(&parsed_statements, &mut tc_ctx) {
        println!("Type check error: {:?}", err);
    }
    let mut frame = StackFrame::new();
    eval_statements(&parsed_statements, &mut frame);
}
