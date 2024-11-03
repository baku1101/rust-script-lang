use nom::Finish;
use rust_script_lang::parser::{eval_statements, statements, StackFrame};
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

        let mut frame = StackFrame::new();
        eval_statements(&parsed_statements, &mut frame);
    }
}
