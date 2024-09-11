use common::*;
mod common;

#[test]
fn group() {
    TestBuild::<GroupUntil<Char, Quote, 0>>::new(
        r#"sing a song of six pence" a pocket"#,
    )
    .pass(Some(r#"sing a song of six pence"#))(
        |_, _| {}
    );
}

#[test]
fn group_unescaped() {
    TestBuild::<GroupUntil<Char, Quote, 0>>::new(
        r#"sing a song\" of six pence" a pocket"#,
    )
    .pass(Some(r#"sing a song\"#))(|_, _| {});
}

#[test]
fn group_escaped() {
    TestBuild::<GroupUntil<StringChars, Quote, 0>>::new(
        r#"sing a song\" of six pence" a pocket"#,
    )
    .pass(Some(r#"sing a song\" of six pence"#))(
        |_, _| {}
    );
}

#[test]
fn group_no_end() {
    TestBuild::<GroupUntil<StringChars, Quote, 0>>::new(
        r#"sing a **\"song of six pence"#,
    )
    .fail()(|_, error| {
        // Error test
        let kind = error.kind;
        if let Kind::NotFound(syntax) = kind {
            assert_eq!(syntax, Quote::NAME)
        }
        else {
            panic!("expecting error of kind `NotFound`");
        }
    });
}

#[test]
fn insufficient_group() {
    TestBuild::<GroupUntil<StringChars, Quote, 5>>::new(
        r#"sing" a **\"song of six pence"#,
    )
    .fail()(|_, error| {
        // Error test
        let kind = error.kind;
        if let Kind::NotFound(syntax) = kind {
            assert_eq!(
                syntax,
                <GroupUntil<StringChars, Quote, 5>>::NAME
            )
        }
        else {
            panic!("expecting error of kind `NotFound`");
        }
    });
}
