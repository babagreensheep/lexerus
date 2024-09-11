use crate::*;

impl<'code, Type> Token<'code>
    for ::core::marker::PhantomData<Type>
where
    Type: Token<'code>,
{
    fn buffer(&self) -> Option<Buffer<'code>> {
        None
    }

    const NAME: &'static str = Type::NAME;
}

impl<'code, Type> Lexer<'code>
    for ::core::marker::PhantomData<Type>
where
    Type: Lexer<'code>,
{
    fn lex(
        buffer: &mut Buffer<'code>,
    ) -> Result<Self, Error<'code>> {
        // Local
        let mut local_buffer = buffer.clone();

        // Try parse?
        let _ = <Type as Lexer>::lex(&mut local_buffer)?;

        // Reset buffer
        *buffer = local_buffer;

        Ok(Self)
    }
}
