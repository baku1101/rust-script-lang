use nom::Finish;
use rust_script_lang::evaluator::{eval_statements, StackFrame};
use rust_script_lang::parser::statements;
use rust_script_lang::typechecker::{type_check, TypeCheckContext};
use std::io::Read;

fn main() {
    let mut buf = String::new();
    if std::io::stdin().read_to_string(&mut buf).is_ok() {
        let parsed_statements = match statements(&buf).finish() {
            Ok((_, parsed_statements)) => parsed_statements,
            Err(e) => {
                eprintln!("Parse error: {e:?}");
                return;
            }
        };

        let mut tc_ctx = TypeCheckContext::new();
        if let Err(err) = type_check(&parsed_statements, &mut tc_ctx) {
            eprintln!("Type check error: {err}");
            return;
        }

        let mut frame = StackFrame::new();
        eval_statements(&parsed_statements, &mut frame);
    }
}
