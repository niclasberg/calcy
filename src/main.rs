use ariadne::{Label, Report, Source};

use crate::{
    eval::EvalContext,
    expr::{ExprView, Expressions, ParseError},
    lexer::{SourceSpan, tokens},
};

mod atom;
mod eval;
mod expr;
mod lexer;
mod types;
mod value;

fn format_error(err: &ParseError) -> Report<'_, SourceSpan> {
    let mut builder = Report::build(ariadne::ReportKind::Error, err.span);
    builder = match &err.kind {
        expr::ParseErrorKind::UnexpectedToken { found, expected } => {
            builder.with_message("Unexpected token").with_label(
                Label::new(err.span)
                    .with_message(format!("Found `{}`, expected `{}`", &found, &expected)),
            )
        }
        expr::ParseErrorKind::UnexpectedEOF => builder.with_message("Unexpected end of file"),
    };
    builder.finish()
}

fn main() {
    let source = "
        type A = #{
            a: Num,
            b: Bool,
        };
        let a = 2 + -2; 
        let k = a + 1; 
        a = fn(a, b) 
            fn(k) (
                let d = a + b + k; 
                [[a, a], b, k, d<a]
            ); 
        a(15, 4)(40)
    ";
    let tokens = tokens(source).unwrap();
    let mut exprs = Expressions::new();
    let expr = match exprs.parse(&tokens) {
        Ok(expr) => expr,
        Err(err) => {
            let report = format_error(&err);
            report.print(Source::from(source)).unwrap();
            return;
        }
    };

    let mut cx = EvalContext::new();
    let value = match eval::eval(expr, &exprs, &mut cx) {
        Ok(value) => value,
        Err(err) => {
            Report::build(ariadne::ReportKind::Error, err.span)
                .with_message(&err.title)
                .with_label(Label::new(err.span).with_message(err.reason))
                .finish()
                .print(Source::from(source))
                .unwrap();
            return;
        }
    };
    println!("{:?} -> {:?}", exprs.view(expr), value);
}
