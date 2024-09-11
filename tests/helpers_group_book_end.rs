use common::*;
mod common;

#[test]
fn group_bookend_escaped() {
    TestBuild::<GroupBookEnd<Quote, StringChars, Quote, 0>>::new(
        r#""asdggasdj" lwearf""#,
    )
    .pass(Some(r#""asdggasdj""#))(|_, _| {});
}

#[test]
fn group_bookend_no_start() {
    TestBuild::<GroupBookEnd<Quote, StringChars, Quote, 0>>::new(
        r#"asdggasdj" lwearf""#,
    )
    .fail()(|_, error| {
        let kind = error.kind;
        if let Kind::NotFound(syntax) = kind {
            assert_eq!(syntax, Quote::NAME);
        }
        else {
            panic!("expecting error of kind `NotFound`")
        }
    });
}
