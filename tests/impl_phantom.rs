use std::marker::PhantomData;

use common::*;
mod common;

#[derive(Lexer, Token, Debug)]
struct Space<'code>(#[pattern(" ")] Buffer<'code>);

#[test]
fn group_phantom() {
    TestBuild::<PhantomData<Group<Space, 0>>>::new(
        r#"          sing a song of six pence a pocket"#,
    )
    .pass(Some(r#""#))(|mut buffer, captured| {
        if Token::buffer(&captured).is_some() {
            panic!("buffer should be empty")
        };

        let result =
            <Char as Lexer>::lex(&mut buffer).unwrap();
        let result = result.buffer().unwrap();
        assert_eq!(result.to_string(), r#"s"#);
    });
}
