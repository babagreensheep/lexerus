//! Whitespace markers
use crate::Buffer;
use crate::Lexer;
use crate::Token;
use std::fmt::Display;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ops::DerefMut;

#[derive(Lexer, Token, Debug, Clone)]
#[lexerus = "crate"]
/// Literal whitespace ` `
pub struct Space<'code>(#[pattern = " "] Buffer<'code>);

#[derive(Lexer, Token, Debug, Clone)]
#[lexerus = "crate"]
/// Literal tab `\t`
pub struct Tab<'code>(#[pattern = "\t"] Buffer<'code>);

#[derive(Lexer, Token, Debug, Clone)]
#[lexerus = "crate"]
/// Literal newline `\n`
pub struct NewLine<'code>(
    #[pattern("\n", "\r")] Buffer<'code>,
);

#[derive(Lexer, Token, Debug, Clone)]
#[lexerus = "crate"]
/// Represents any permutation of possible white
/// spaces. Could be ` `, `\t`, `\n` or whatver
/// whitespaces are subsequently added.
pub enum WhiteSpace<'code> {
    Space(Space<'code>),
    Tab(Tab<'code>),
    NewLine(NewLine<'code>),
}

/// Unallocated [crate::Group] of [WhiteSpace].
/// This can probably be used in the context of
/// breaking up `SYMBOL [WhiteSpaces] SYMBOL` in
/// complex chains.
pub type WhiteSpaces<'code> =
    PhantomData<super::Group<'code, WhiteSpace<'code>, 1>>;

#[derive(Clone, Debug)]
/// Use [Token] to wrap any `Type`::[Lexer] to try to
/// consume all [WhiteSpaces] **before** and **after**
/// the given [Lexer]
///
/// **WARNING**:
/// This should generally be used only when trying to
/// group base tokens together. If we wanted to put
/// together `[`, `TOKEN` and `]` for example, we
/// would want to wrap each of the parenthesis with
/// [Isolate] to ensure that `[  TOKEN ]` is validly
/// evaluated as well. However there is no need to wrap
/// `TOKEN` itself.
pub struct Isolate<Type> {
    pub token: Type,
}

impl<Type: std::fmt::Display> Display for Isolate<Type> {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        Display::fmt(&self.token, f)
    }
}

impl<Type> AsMut<Type> for Isolate<Type> {
    fn as_mut(&mut self) -> &mut Type {
        &mut self.token
    }
}

impl<Type> AsRef<Type> for Isolate<Type> {
    fn as_ref(&self) -> &Type {
        &self.token
    }
}

impl<Type> DerefMut for Isolate<Type> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.token
    }
}

impl<Type> Deref for Isolate<Type> {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}

impl<'code, Type: Token<'code>> Token<'code>
    for Isolate<Type>
{
    const NAME: &'static str = Type::NAME;

    fn buffer(&self) -> Option<Buffer<'code>> {
        self.token.buffer()
    }
}

impl<'code, Type: Lexer<'code>> Lexer<'code>
    for Isolate<Type>
{
    fn lex(
        buffer: &mut crate::Buffer<'code>,
    ) -> Result<Self, crate::Error<'code>> {
        let mut local_buffer = buffer.clone();
        let count: ::std::primitive::usize = 0;
        let _ = <PhantomData<
            Option<WhiteSpaces<'code>>,
        > as crate::Lexer>::lex(
            &mut local_buffer
        );
        let token =
            <Type as crate::Lexer>::lex(&mut local_buffer)
                .map_err(
                    |mut err: crate::Error<'code>| {
                        err.matched += count;
                        err
                    },
                )?;
        let _ = <PhantomData<
            Option<WhiteSpaces<'code>>,
        > as crate::Lexer>::lex(
            &mut local_buffer
        );
        *buffer = local_buffer;
        let ok_value = Self { token };
        Ok::<Self, crate::Error<'code>>(ok_value)
    }
}
