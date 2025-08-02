use proc_macro2::TokenStream;
use quote::ToTokens;
use quote::format_ident;
use quote::quote;
use quote::quote_spanned;
use std::fmt::Debug;
use syn::Attribute;
use syn::Block;
use syn::DataEnum;
use syn::DataStruct;
use syn::DeriveInput;
use syn::Error;
use syn::Expr;
use syn::ExprArray;
use syn::ExprBlock;
use syn::ExprIf;
use syn::ExprLet;
use syn::ExprMatch;
use syn::Field;
use syn::FieldValue;
use syn::Fields;
use syn::FieldsUnnamed;
use syn::GenericParam;
use syn::Generics;
use syn::Ident;
use syn::ItemFn;
use syn::ItemImpl;
use syn::Lifetime;
use syn::Lit;
use syn::LitInt;
use syn::LitStr;
use syn::Meta;
use syn::Token;
use syn::Type;
use syn::TypeArray;
use syn::TypePath;
use syn::TypeTuple;
use syn::Variant;
use syn::parse::Parser;
use syn::parse2;
use syn::punctuated;
use syn::spanned::Spanned;

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
        Ok(mut parse_impl) => {
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
