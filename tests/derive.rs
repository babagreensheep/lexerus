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

#[derive(Lexer, Token, Debug)]
struct Quote<'code>(#[pattern = "\""] Buffer<'code>);

#[test]
fn eat_before() {
    #[derive(Lexer, Token, Debug)]
    #[before(Quote)]
    struct Trex<'code>(#[pattern = "rawr"] Buffer<'code>);

    lexerus::TestBuild::<Trex>::new(
        r#""rawr i have a big head"#,
    )
    .pass(Some(r#"rawr"#))(|_, _| {});
}

#[test]
fn eat_before_failt() {
    #[derive(Lexer, Token, Debug)]
    #[before(Quote)]
    struct Trex<'code>(#[pattern = "rawr"] Buffer<'code>);

    lexerus::TestBuild::<Trex>::new(
        r#"rawr i have a big head"#,
    )
    .fail()(|_, err| {
        assert_eq!(err.kind, Kind::NotFound("Quote"))
    });
}

#[test]
fn eat_after() {
    #[derive(Lexer, Token, Debug)]
    #[after(Quote)]
    struct Trex<'code>(#[pattern = "rawr"] Buffer<'code>);

    lexerus::TestBuild::<Trex>::new(
        r#"rawr" i have a big head"#,
    )
    .pass(Some(r#"rawr"#))(|buffer, captured| {
        assert_eq!(
            buffer.to_string(),
            " i have a big head"
        );
    });
}

#[test]
fn eat_after_fail() {
    #[derive(Lexer, Token, Debug)]
    #[after(Quote)]
    struct Trex<'code>(#[pattern = "rawr"] Buffer<'code>);

    lexerus::TestBuild::<Trex>::new(
        r#"rawr i have a big head"#,
    )
    .fail()(|_, err| {
        assert_eq!(err.kind, Kind::NotFound("Quote"))
    });
}

#[test]
fn eat_around() {
    #[derive(Lexer, Token, Debug)]
    struct Ra<'code>(#[pattern = "ra"] Buffer<'code>);

    #[derive(Lexer, Token, Debug)]
    #[before(Quote, Ra)]
    #[after(Quote)]
    struct Wr<'code>(#[pattern = "wr"] Buffer<'code>);

    lexerus::TestBuild::<Wr>::new(
        r#""rawr" i have a big head"#,
    )
    .pass(Some(r#"wr"#))(|buffer, captured| {
        assert_eq!(
            buffer.to_string(),
            " i have a big head"
        );
    });
}
