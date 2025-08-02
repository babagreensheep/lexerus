#![feature(iter_intersperse)]
//! Macros for the [crate]
use proc_macro2::TokenStream;

mod entrypoint;

#[proc_macro_derive(
    Lexer,
    attributes(
        pattern, before, after, hook, lexerus, build_with
    )
)]
/// # [Lexer]
/// Decorates a `struct` or `enum` with `impl Lexer<'code>`.
///
/// # Attributes
///
/// ## Outer
///
/// ### Lexerus
/// Remaps the module name
/// ```ignore
/// #[lexerus = "newname"]
/// ```
///
/// ### Hook
/// A hook to run after the [Lexer] has successfullly lexed and produced a [Result::Ok]
/// ```ignore
/// #[derive(Lexer, Token, Debug)]
/// struct Trex<'code>(#[pattern = "rawr"] Buffer<'code>);
///
/// #[derive(Lexer, Token, Debug)]
/// #[hook(Self::hook)]
/// struct TaggedTrex<'code>(
///     Trex<'code>,
///     #[build_with(Ok(0))] usize,
/// );
///
/// impl<'code> TaggedTrex<'code> {
///     fn hook(mut self) -> Result<Self, Error<'code>> {
///         self.1 = 10;
///         Ok(self)
///     }
/// }
/// ```
///
/// ### Before / After
/// Eats tokens before and after
/// ```ignore
/// #[derive(Lexer, Token, Debug)]
/// struct Ra<'code>(#[pattern = "ra"] Buffer<'code>);
///
/// #[derive(Lexer, Token, Debug)]
/// #[before(Quote, Ra)]
/// #[after(Quote)]
/// struct Wr<'code>(#[pattern = "wr"] Buffer<'code>);
/// ```
///
/// ## Inner
///
/// ### Pattern
/// Either of the following can be used to mark a field which
/// contains a buffer, which tells the lexer to match `pattern`
/// at the **start** of the buffer
///  - `#[pattern = "pat"]`: Matches `pat`
///  - `#[pattern("pat1", "pat2")]`: Matchines `pat1` or
///    `pat2`
/// ```ignore
/// #[derive(Debug, Clone, Lexer, Token)]
/// pub struct LcLetter<'code>(
///     #[pattern(
///         "a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
///         "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
///         "u", "v", "w", "x", "y", "z"
///     )]
///     Buffer<'code>,
/// );
/// ```
///
/// ### Build with
/// Specify an expression which is used to build the annotated field
/// Remaps the module name
/// ```ignore
/// #[derive(Lexer, Token, Debug)]
/// struct Trex<'code>(#[pattern = "rawr"] Buffer<'code>);
///
/// #[derive(Lexer, Token, Debug)]
/// struct BuiltRex<'code>(
///     Trex<'code>,
///     #[build_with(Ok(0))] usize,
/// );
/// ```
pub fn derive_lex(
    expr: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    entrypoint::entrypoint(
        TokenStream::from(expr),
        entrypoint::Macro::Lex,
    )
    .into()
}

#[proc_macro_derive(
    Token,
    attributes(pattern, lexerus, ignore_token, build_with)
)]
/// Decorates a `struct` or `enum` with `impl Token<'code>`.
///
/// **Attributes**:
///
/// - `lexerus`: Remap for the module name
///
/// - `ignore_token` / `build_with`: Syntax and Buffer will
///   not be generated.
///
/// - `pattern`: Either of the following can be used to mark
///   a field which contains a buffer, which tells the
///   [Token] to return `pattern` as the output of the
///   syntax igenerator
pub fn derive_token(
    expr: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    entrypoint::entrypoint(
        TokenStream::from(expr),
        entrypoint::Macro::Token,
    )
    .into()
}
