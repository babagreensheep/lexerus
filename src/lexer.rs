use super::*;

pub trait Lexer<'code>
where
    Self: Sized + Token<'code>,
{
    fn lex(
        buffer: &mut Buffer<'code>,
    ) -> Result<Self, Error<'code>>;
}
