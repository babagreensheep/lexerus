use crate::*;

impl<'code, First> Token<'code> for Option<First>
where
    First: Token<'code>,
{
    const NAME: &'static str = <First as Token>::NAME;

    fn buffer(&self) -> Option<Buffer<'code>> {
        match self {
            Some(value) => <First as Token>::buffer(value),
            None => None,
        }
    }
}

impl<'code, First> Lexer<'code> for Option<First>
where
    First: Lexer<'code>,
{
    fn lex(
        buffer: &mut Buffer<'code>,
    ) -> Result<Self, Error<'code>> {
        // Clone buffer so that we dont move pointer if
        // there is an error
        let mut local_buffer = buffer.clone();

        // Try to create token
        let first =
            <First as Lexer>::lex(&mut local_buffer);

        // Reset buffer
        if first.is_ok() {
            *buffer = local_buffer;
        }

        // Return token
        Ok(first.ok())
    }
}
