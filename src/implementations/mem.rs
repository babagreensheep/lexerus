use crate::*;

impl<'code, First> Token<'code> for Box<First>
where
    First: Token<'code>,
{
    const NAME: &'static str = <First as Token>::NAME;

    fn buffer(&self) -> Option<Buffer<'code>> {
        let value = self.as_ref();
        <First as Token>::buffer(value)
    }
}

impl<'code, First> Lexer<'code> for Box<First>
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
            <First as Lexer>::lex(&mut local_buffer)?;

        // Reset buffer
        *buffer = local_buffer;

        Ok(first.into())
    }
}

impl<'code, First> Token<'code> for std::sync::Arc<First>
where
    First: Token<'code>,
{
    const NAME: &'static str = <First as Token>::NAME;

    fn buffer(&self) -> Option<Buffer<'code>> {
        let value = self.as_ref();
        <First as Token>::buffer(value)
    }
}

impl<'code, First> Lexer<'code> for std::sync::Arc<First>
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
            <First as Lexer>::lex(&mut local_buffer)?;

        // Reset buffer
        *buffer = local_buffer;

        Ok(first.into())
    }
}

impl<'code, First> Token<'code> for std::rc::Rc<First>
where
    First: Token<'code>,
{
    const NAME: &'static str = <First as Token>::NAME;

    fn buffer(&self) -> Option<Buffer<'code>> {
        let value = self.as_ref();
        <First as Token>::buffer(value)
    }
}

impl<'code, First> Lexer<'code> for std::rc::Rc<First>
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
            <First as Lexer>::lex(&mut local_buffer)?;

        // Reset buffer
        *buffer = local_buffer;

        Ok(first.into())
    }
}
