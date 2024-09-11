#![allow(dead_code)]
//! Mark EOF
use crate::*;
use ::std::marker::PhantomData;

#[derive(Clone)]
/// Lookahead to see if a [Token] exists.
/// **Does not consume the buffer** and as such is a very
/// useful tool for checking that the specified [Token]
/// exists as a form of validation, but letting the
/// [Lexer] on the next [Token] process it instead.
pub struct Peek<Type>(PhantomData<Type>);

impl<Type> ::std::fmt::Debug for Peek<Type> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        f.write_str("..")
    }
}

impl<'code, Type> Lexer<'code> for Peek<Type>
where
    Type: Lexer<'code>,
{
    fn lex(
        buffer: &mut Buffer<'code>,
    ) -> Result<Self, Error<'code>> {
        // Clone buffer
        let mut buffer = buffer.clone();

        match <Type as Lexer>::lex(&mut buffer) {
            Ok(_) => Ok(Self(PhantomData)),
            Err(err) => Err(err),
        }
    }
}

impl<'code, Type> Token<'code> for Peek<Type>
where
    Type: Token<'code>,
{
    const NAME: &'static str = <Type as Token>::NAME;

    fn buffer(&self) -> Option<Buffer<'code>> {
        None
    }
}
