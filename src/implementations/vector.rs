use crate::*;

impl<'code, First> Token<'code> for Vec<First>
where
    First: Token<'code>,
{
    fn buffer(&self) -> Option<Buffer<'code>> {
        let buffers = self
            .iter()
            .map(|b| <First as Token>::buffer(b));

        buffers
            .flatten()
            .reduce(|acc, current| acc + current)
    }

    const NAME: &'static str = "Vec";
}

impl<'code, First> Lexer<'code> for Vec<First>
where
    First: Lexer<'code>,
{
    fn lex(
        buffer: &mut Buffer<'code>,
    ) -> Result<Self, Error<'code>> {
        // Clone buffer so that we dont move pointer if
        // there is an error
        let mut local_buffer = buffer.clone();

        // Create vector
        let mut result = Vec::new();

        // Voraciously consume tokens
        // Push results
        while let Ok(token) =
            <First as Lexer>::lex(&mut local_buffer)
        {
            result.push(token);
        }

        // Reset buffer
        *buffer = local_buffer;

        Ok(result)
    }
}
