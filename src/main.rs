use ariadne::{Label, Report, Source};

use crate::{
    eval::EvalContext,
    expr::Expressions,
    lexer::{SourceSpan, tokens},
    parse_expr::{ParseError, ParseErrorKind, parse},
    types::{TypeContext, TypeError, TypeErrorKind, infer},
};

mod atom;
mod eval;
mod expr;
mod lexer;
mod parse_expr;
mod types;
mod value;

fn format_parse_error(err: &ParseError) -> Report<'_, SourceSpan> {
    let mut builder = Report::build(ariadne::ReportKind::Error, err.span);
    builder = match &err.kind {
        ParseErrorKind::UnexpectedToken { found, expected } => {
            builder.with_message("Unexpected token").with_label(
                Label::new(err.span)
                    .with_message(format!("Found `{}`, expected `{}`", &found, &expected)),
            )
        }
        ParseErrorKind::UnexpectedEOF => builder.with_message("Unexpected end of file"),
    };
    builder.finish()
}

fn format_type_error<'s>(err: &TypeError, expressions: &'s Expressions) -> Report<'s, SourceSpan> {
    let mut builder = Report::build(ariadne::ReportKind::Error, err.span);
    builder = match &err.kind {
        TypeErrorKind::UndefinedVariable(atom) => builder
            .with_message("Undefined variable")
            .with_label(Label::new(err.span).with_message(format!(
                "Could not find the variable `{}` in the current scope",
                expressions.get_atom(*atom)
            ))),
        TypeErrorKind::NeedTypeAnnotation => {
            builder.with_message("Type annotation needed").with_label(
                Label::new(err.span)
                    .with_message(format!("Could not infer type, need type annotation")),
            )
        }
        TypeErrorKind::ArgumentCountMismatch { expected, found } => builder
            .with_message("Invalid function call")
            .with_label(Label::new(err.span).with_message(format!(
                "Function expected `{}` arguments, found `{}`",
                expected, found
            ))),
        TypeErrorKind::ExpectedIdentifier => todo!(),
        TypeErrorKind::UnexpectedType { expected, actual } => builder
            .with_message("Type error")
            .with_label(Label::new(err.span).with_message(format!(
                "Exprected type `{}` found `{}`",
                &expected, &actual
            ))),
        TypeErrorKind::ExpectedFunction { found } => {
            builder.with_message("Invalid function call").with_label(
                Label::new(err.span)
                    .with_message(format!("Expected a function, found `{}`", &found)),
            )
        }
        TypeErrorKind::ExpectedArray { found } => builder.with_message("Type error").with_label(
            Label::new(err.span).with_message(format!("Expected an array, found `{}`", &found)),
        ),
    };
    builder.finish()
}

fn main() {
    let source = "
        let a = 2 + -2; 
        let k = a + 1; 
        let dd = true;
        dd = false;
        let f = fn(a: Float, b: Float): (Float) => [Bool | Float] (
            fn(k): [Float] (
                let d = a + b + k; 
                let arr = [10, true];
                [b, k, d<a, ..arr, false]
            )
        ); 
        if a < k then f(15, 4)(40) else [true]
    ";
    let tokens = tokens(source).unwrap();
    let mut exprs = Expressions::new();
    let expr = match parse(&mut exprs, &tokens) {
        Ok(expr) => expr,
        Err(err) => {
            let report = format_parse_error(&err);
            report.print(Source::from(source)).unwrap();
            return;
        }
    };

    let mut type_cx = TypeContext::new();
    let expr_type = match infer(&mut type_cx, &exprs, expr) {
        Ok(expr_type) => expr_type,
        Err(type_error) => {
            let report = format_type_error(&type_error, &exprs);
            report.print(Source::from(source)).unwrap();
            return;
        }
    };
    println!("Result type: {}", expr_type);

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
    println!("{:?} -> {}", exprs.view(expr), value);
}
