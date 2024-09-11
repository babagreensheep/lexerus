use common::*;
mod common;

#[test]
fn simple_not() {
    TestBuild::<Not<Quote>>::new(
        r#"sing a song\" of six pence" a pocket"#,
    )
    .pass(Some(r#"s"#))(|_, _| {});
}
