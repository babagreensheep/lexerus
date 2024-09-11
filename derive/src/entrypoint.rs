use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned, ToTokens};
use std::fmt::Debug;
use syn::{
    parse::Parser, parse2, punctuated, spanned::Spanned,
    Attribute, Block, DataEnum, DataStruct, DeriveInput,
    Error, Expr, ExprArray, ExprBlock, ExprIf, ExprLet,
    ExprMatch, Field, FieldValue, Fields, FieldsUnnamed,
    Generics, Ident, ItemFn, ItemImpl, Lifetime, Lit,
    LitInt, LitStr, Token, Type, TypeArray, TypePath,
    TypeTuple, Variant,
};

// ImplParse
mod lexer_impl;

pub enum Macro {
    Lex,
    Token,
}

/// Entrypoint
pub fn entrypoint(
    expr: TokenStream,
    macro_type: Macro,
) -> TokenStream {
    let expr = lexer_impl::LexerImpl::try_from(expr);

    match expr {
        Ok(parse_impl) => {
            // Try to create tokens
            match macro_type {
                Macro::Lex => parse_impl.impl_trait_lex(),
                Macro::Token => {
                    parse_impl.impl_trait_token()
                }
            }
            .unwrap_or_else(|err| err.into_compile_error())
        }
        Err(err) => err.into_compile_error(),
    }
}

// tests
#[cfg(test)]
mod tests;
