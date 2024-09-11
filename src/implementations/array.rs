use crate::*;

impl<'code, First, const N: usize> Token<'code>
    for [First; N]
where
    First: Token<'code>,
{
    const NAME: &'static str = <First as Token>::NAME;

    fn buffer(&self) -> Option<Buffer<'code>> {
        let buffers = self
            .iter()
            .map(|b| <First as Token>::buffer(b));

        buffers
            .flatten()
            .reduce(|acc, current| acc + current)
    }
}

impl<'code, First, const N: usize> Lexer<'code>
    for [First; N]
where
    First: Lexer<'code>,
{
    fn lex(
        buffer: &mut Buffer<'code>,
    ) -> Result<Self, Error<'code>> {
        let mut local_buffer = buffer.clone();

        // Create result
        let mut matched = 0usize;
        let results: [First; N] =
            std::array::try_from_fn(|_| {
                match <First as Lexer>::lex(
                    &mut local_buffer,
                ) {
                    Ok(result) => {
                        matched += 1;
                        Ok(result)
                    }
                    Err(mut err) => {
                        err.matched = matched;
                        Err(err)
                    }
                }
            })?;

        // Adjust buffer
        *buffer = local_buffer;

        // Return result
        Ok(results)
    }
}
