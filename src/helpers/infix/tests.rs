use crate::*;

#[derive(Lexer, Token, Debug)]
#[lexerus = "crate"]
struct Plus<'code>(#[pattern = "+"] Buffer<'code>);
#[derive(Lexer, Token, Debug)]
#[lexerus = "crate"]
struct Number<'code>(#[pattern = "12"] Buffer<'code>);

#[test]
fn simple_infix_constructor() {
    TestBuild::<Infix<Number, Plus, Number>>::new("12+12")
        .pass(Some("12+12"))(|_buffer, capture| {
        println!("{capture:#?}");
    });
}

#[test]
fn non_infix() {
    TestBuild::<Infix<Number, Plus, Number>>::new("12")
        .pass(Some("12"))(|_buffer, capture| {
        println!("{capture:#?}");
    });
}

#[test]
fn fail_infix() {
    TestBuild::<Infix<Number, Plus, Number>>::new("12+")
        .fail()(|_buffer, capture| {
        println!("{capture:#?}");
    });
}
