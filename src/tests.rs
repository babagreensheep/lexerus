use super::*;

const INFO: &str = "[\x1b[32mINFO \x1b[0m]";
const ERROR: &str = "[\x1b[31mERROR\x1b[0m]";

/// Test harness for tokens.
pub struct TestBuild<'code, Capture>
where
    Capture: Lexer<'code>,
{
    original_code: &'code str,
    buffer: Buffer<'code>,
    _c: ::std::marker::PhantomData<Capture>,
}

impl<'code, Capture> TestBuild<'code, Capture>
where
    Capture: Lexer<'code> + std::fmt::Debug,
{
    /// Create a new test harness to parse the supplied code.
    pub fn new(code: &'code str) -> Self {
        Self {
            original_code: code,
            buffer: code.into(),
            _c: ::std::marker::PhantomData,
        }
    }

    fn capture(
        buffer: &mut Buffer<'code>,
    ) -> Result<Capture, Error<'code>>
    where
        Capture: Lexer<'code>,
    {
        println!("{INFO} Buffer: {buffer}");
        println!(
            "{INFO} Token name: {}",
            <Capture as Token>::NAME
        );
        <Capture as Lexer>::lex(buffer)
    }

    /// Telegraphs to the harness that you expect a failure. Returns a function that can be called
    /// with a callback to examine the [Result::Err].
    pub fn fail(
        self,
    ) -> impl FnOnce(fn(Buffer<'code>, Error<'code>)) {
        let mut buffer = self.buffer;
        let captured =
            Self::capture(&mut buffer).unwrap_err();
        println!("{ERROR} Error: {:?}", captured);
        assert_eq!(self.original_code, buffer.to_string());
        move |cb: fn(Buffer<'code>, Error<'code>)| {
            cb(buffer, captured);
        }
    }

    /// Telegraphs to the harness that you expect a pass. Returns a function that can be called
    /// with a callback to examine the [Result::Ok]
    pub fn pass(
        self,
        success: Option<&str>,
    ) -> impl FnOnce(fn(Buffer<'code>, Capture)) {
        let mut buffer = self.buffer;
        let captured = Self::capture(&mut buffer).unwrap();
        println!("{INFO} Captured: {:#?}", captured);
        if let Some(captured_buffer) = &captured.buffer() {
            if let Some(success) = success {
                assert_eq!(
                    captured_buffer.to_string(),
                    success
                );
                println!(
                    "{INFO} Matched: {captured_buffer:?}",
                );
            }
        }
        move |cb: fn(Buffer<'code>, Capture)| {
            cb(buffer, captured);
        }
    }
}
