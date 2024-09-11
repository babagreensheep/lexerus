use common::*;
use lexerus::Searchable;
mod common;

#[derive(Lexer, Token, Debug)]
struct Alpha<'code>(
    #[pattern("s", "i", "n")] Buffer<'code>,
);

#[test]
fn group_till_end() {
    TestBuild::<Group<Char, 0>>::new(
        r#"sing a song of six pence a pocket"#,
    )
    .pass(Some(r#"sing a song of six pence a pocket"#))(
        |_, _| {},
    );
}

#[test]
fn group_till_sin() {
    TestBuild::<Group<Alpha, 0>>::new(
        r#"sing a song of six pence a pocket"#,
    )
    .pass(Some(r#"sin"#))(|_, _| {});
}

#[test]
fn group_sufficient() {
    // Should pass
    TestBuild::<Group<Alpha, 3>>::new(
        r#"sing a song of six pence a pocket"#,
    )
    .pass(Some(r#"sin"#))(|_, _| {});

    // Should fail
    TestBuild::<Group<Alpha, 4>>::new(
        r#"sing a song of six pence a pocket"#,
    )
    .fail()(|_, err| {
        assert_eq!(err.matched, 3);
        if let Kind::NotFound(syntax) = err.kind {
            assert_eq!(syntax, Alpha::NAME);
        }
        else {
            panic!("wrong error return");
        }
    });
}
