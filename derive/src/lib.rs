#![feature(iter_intersperse)]
//! Macros for the [crate]
use proc_macro2::TokenStream;

mod entrypoint;

#[proc_macro_derive(
    Lexer,
    attributes(pattern, hook, lexerus, build_with)
)]
/// Decorates a `struct` or `enum` with `impl Lexer<'code>`.
///
/// **Attributes**:
///
/// - `lexerus`: Remap for the module name
///
/// - `pattern`: Either of the following can be used to mark
///   a field which contains a buffer, which tells the lexer
///   to match `pattern` at the **start** of the buffef
///     - `#[pattern = "pat"]`: Matches `pat`
///     - `#[pattern("pat1", "pat2")]`: Matchines `pat1` or
///       `pat2`
///
/// - `build_with`: Specify an expression which is used to
///   build the annotated field
///
/// - `hook(function)`: Annotates the structure to tell
///   [Lexer] to run the ok result through the hook first.
///   The `function` must accept `self` as its only
///   parameter, and return `Result<Self, Error<'code>>`
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
