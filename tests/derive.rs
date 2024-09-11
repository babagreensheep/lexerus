pub use lexerus::*;

#[derive(Lexer, Token, Debug)]
struct Trex<'code>(#[pattern = "rawr"] Buffer<'code>);

#[derive(Lexer, Token, Debug)]
#[hook(Self::hook)]
struct TaggedTrex<'code>(
    Trex<'code>,
    #[build_with(Ok(0))] usize,
);

impl<'code> TaggedTrex<'code> {
    fn hook(mut self) -> Result<Self, Error<'code>> {
        self.1 = 10;
        Ok(self)
    }
}

#[derive(Lexer, Token, Debug)]
struct IgnoredTrex<'code>(
    Trex<'code>,
    #[allow(dead_code)]
    #[ignore_token]
    Trex<'code>,
);

#[test]
fn lexer_hook() {
    lexerus::TestBuild::<TaggedTrex>::new(
        r#"rawr i have a big head"#,
    )
    .pass(Some(r#"rawr"#))(|_, captured| {
        assert_eq!(captured.1, 10)
    });
}

#[test]
fn build_with() {
    lexerus::TestBuild::<TaggedTrex>::new(
        r#"rawr i have a big head"#,
    )
    .pass(Some(r#"rawr"#))(|_, _| {});
}

#[test]
fn ignore_token() {
    lexerus::TestBuild::<IgnoredTrex>::new(
        r#"rawrrawr i have a big head"#,
    )
    .pass(Some(r#"rawr"#))(|_, _| {});
}
