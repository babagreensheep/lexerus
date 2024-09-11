use super::*;

/// Trait that indicates that the structure is able to be constructed from a [Buffer] by calling
/// [Lexer::lex] on the struct.
pub trait Lexer<'code>
where
    Self: Sized + Token<'code>,
{
    /// Actual method that is used to lex a [Buffer]
    ///
    /// **Warning**: The lexer operates by mutating a reference to a [Buffer]. If you are
    /// handrolling this structure, remember that the [Lexer::lex] function for the constituent
    /// tokens should _not_ be called directly on the [Buffer] reference. If lexing the
    /// constitutent tokens fails, the [Buffer]'s internal pointer would have nonetheless been
    /// advanced meaning that the caller skips an unparsed token unwittingly. As such, all
    /// handrolled implementations will clone the [Buffer] as a local variable and update the [Buffer]
    /// only when the constitution tokens have been successfully constructed. See [Isolate]'s
    /// implementation for example.
    fn lex(
        buffer: &mut Buffer<'code>,
    ) -> Result<Self, Error<'code>>;
}
