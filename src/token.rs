use super::*;

/// Trait indicating that the struct is ready for [Lexer]. This is not included as part of [Lexer]
/// as there are instances where the user may wish to handroll the [Token] representation. In any
/// case they refer to distincting things: [Token] represents the outward represntation of the
/// struct after parsing.
pub trait Token<'code>
where
    Self: Sized,
{
    /// Name of the token
    const NAME: &'static str;

    /// Underlying [Buffer] on the [Token]. This is an [Option] because certain types of
    /// structures, e.g. [std::marker::PhantomData] do not actually store a [Buffer] and it is
    /// therefore theoretically possible that a [Token] has no inherent [Buffer]. Examples include
    /// [Not] and [Peek]
    fn buffer(&self) -> Option<Buffer<'code>>;
}
