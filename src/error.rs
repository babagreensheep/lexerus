use std::fmt::Debug;

use super::*;

#[derive(Debug)]
/// Information about the error which occured when the [Lexer::lex] failed.
pub struct Error<'code> {
    pub buffer: Buffer<'code>,
    /// This is included not just for debug information, but also to allow [lexerus_derive::Lexer]
    /// to obtain some basic information about how many tokens were matched before the program
    /// failed. This is useful when debugging `enum` because it would not be immediately obvious
    /// which match pattern failed.
    pub matched: usize,
    pub kind: Kind,
}

#[derive(Debug, PartialEq, Eq)]
/// Describes the type of error found.
pub enum Kind {
    /// This means that the token was not found
    NotFound(&'static str),
    /// This means that an unexpected token was encountered.
    UnexpectedToken(&'static str),
}
