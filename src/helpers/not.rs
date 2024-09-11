//! Not helper type used to capture the inverse of
//! something

// Crate imports
use crate::*;
use ::std::marker::PhantomData;

#[derive(Clone)]
/// Matches only if the next token is `Type`. Uses
/// [PhantomData] and does not capture the `Type` nor the
/// underlying [Buffer]
pub struct Not<Type> {
    phantom: PhantomData<Type>,
}

impl<Type> std::fmt::Debug for Not<Type> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        f.debug_struct("Not").finish()
    }
}

impl<'code, First> Token<'code> for Not<First>
where
    First: Token<'code>,
{
    const NAME: &'static str = "Not";

    fn buffer(&self) -> Option<Buffer<'code>> {
        None
    }
}

impl<'code, First> Lexer<'code> for Not<First>
where
    First: Lexer<'code>,
{
    fn lex(
        buffer: &mut Buffer<'code>,
    ) -> Result<Self, Error<'code>> {
        // Local
        let mut local_buffer = buffer.clone();

        // Create vector
        if let Ok(_token) =
            <First as Lexer>::lex(&mut local_buffer)
        {
            Err(Error {
                buffer: buffer.clone(),
                matched: 0,
                kind: Kind::UnexpectedToken(
                    <First as Token>::NAME,
                ),
            })
        }
        else {
            Ok(Self {
                phantom: PhantomData,
            })
        }
    }
}
