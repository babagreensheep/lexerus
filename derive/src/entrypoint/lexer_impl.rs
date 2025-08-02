//! Helper for implying [#module_name::Lexer] trait.
use syn::Stmt;
use syn::parse::ParseStream;
use syn::punctuated::Punctuated;

use super::*;

enum Data {
    Struct(DataStruct),
    Enum(DataEnum),
}

/// Interim parser
pub(crate) struct LexerImpl {
    ident: Ident,
    module_name: Ident,
    lexer_hook: Option<Expr>,
    new_lifetime: Lifetime,
    new_generics: Generics,
    generics: Generics,
    data: Data,
    before: Option<Vec<Type>>,
    after: Option<Vec<Type>>,
}

impl Debug for LexerImpl {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        let generics = format!(
            "{}",
            self.generics.clone().into_token_stream()
        );
        let data = match &self.data {
            Data::Struct(data) => {
                data.fields.clone().into_token_stream()
            }
            Data::Enum(data) => {
                data.variants.clone().into_token_stream()
            }
        };
        let data = format! {"{}", data};
        f.debug_struct("LexerImpl")
            .field("ident", &self.ident)
            .field("generics", &generics)
            .field("data", &data)
            .finish()
    }
}

impl LexerImpl {
    fn impl_trait(&self) -> Result<TokenStream, Error> {
        let token_trait_impl = self.impl_trait_token()?;
        let lexer_trait_impl = self.impl_trait_lex()?;

        // Block
        Ok(quote! {
            #token_trait_impl
            #lexer_trait_impl
        })
    }

    pub fn impl_trait_lex(
        &self,
    ) -> Result<TokenStream, Error> {
        let LexerImpl {
            ident,
            generics,
            module_name,
            new_lifetime,
            new_generics,
            ..
        } = self;

        // Get generics
        let (_, impl_type, impl_where) =
            generics.split_for_impl();
        let (impl_gen, _, _) =
            new_generics.split_for_impl();

        // Create impl block
        let lexer_trait_block = self.lexer_trait_block()?;

        // Create impl
        let lexer_trait_impl = quote_spanned! { ident.span() =>
            impl #impl_gen #module_name::Lexer<#new_lifetime> for #ident #impl_type #impl_where #lexer_trait_block
        };

        let lexer_trait_impl =
            parse2::<ItemImpl>(lexer_trait_impl)?;

        // Block
        Ok(quote_spanned! { ident.span() =>
            #lexer_trait_impl
        })
    }

    pub fn impl_trait_token(
        &self,
    ) -> Result<TokenStream, Error> {
        let LexerImpl {
            ident,
            generics,
            module_name,
            new_lifetime,
            new_generics,
            ..
        } = self;

        // Get generics
        let (_, impl_type, impl_where) =
            generics.split_for_impl();
        let (impl_gen, _, _) =
            new_generics.split_for_impl();

        // Create impl block
        let token_trait_block = self.token_trait_block()?;

        // Create impl
        let token_trait_impl = quote_spanned! { ident.span() =>
            impl #impl_gen #module_name::Token<#new_lifetime> for #ident #impl_type #impl_where #token_trait_block
        };

        let token_trait_impl =
            parse2::<ItemImpl>(token_trait_impl)?;

        // Block
        Ok(quote_spanned! { ident.span() =>
            #token_trait_impl
        })
    }

    fn token_trait_block(&self) -> Result<Block, Error> {
        let impl_buffer_fn = self.fn_buffer()?;

        let ident = &self.ident.to_string();

        let block = quote! {{
            const NAME: &'static str = #ident;

            #impl_buffer_fn
        }};
        let block = parse2::<Block>(block)?;
        Ok(block)
    }

    fn lexer_trait_block(&self) -> Result<Block, Error> {
        let impl_lexer_fn = self.fn_lexer()?;

        let block = quote! {{
            #impl_lexer_fn
        }};
        let block = parse2::<Block>(block)?;
        Ok(block)
    }

    fn fn_buffer(&self) -> Result<ItemFn, Error> {
        let block = self.fn_buffer_block()?;
        let new_lifetime = &self.new_lifetime;
        let module_name = &self.module_name;
        let item_fn = quote! {
            fn buffer(&self) -> Option<#module_name::Buffer<#new_lifetime>> #block
        };
        let item_fn = parse2::<ItemFn>(item_fn)?;
        Ok(item_fn)
    }

    fn fn_buffer_block(&self) -> Result<Block, Error> {
        let data = &self.data;
        let block = match data {
            Data::Struct(data) => {
                let ident = format_ident!("self");
                let expr_array = self
                    .fn_buffer_array_from_fields::<true>(
                        Some(&ident),
                        &data.fields,
                    )?;
                self.buffer_block_from_array(&expr_array)?
            }
            Data::Enum(data) => self
                .fn_buffer_block_from_enum(
                    data.variants.iter(),
                )?,
        };
        Ok(block)
    }

    fn fn_buffer_block_from_enum<'variant, Variants>(
        &self,
        variants: Variants,
    ) -> Result<Block, Error>
    where
        Variants: Iterator<Item = &'variant Variant>,
    {
        let match_arms = variants
            .map(|variant| {
                let Variant { ident, fields, .. } = variant;

                let mut named = true;
                let match_type = quote! {Self::#ident};
                let match_type =
                    parse2::<Type>(match_type)?;

                let mut members = Vec::<Ident>::new();
                let mut fields = fields.clone();
                for (field_counter, field) in
                    fields.iter_mut().enumerate()
                {
                    match &field.ident {
                        Some(ident) => {
                            members.push(ident.clone())
                        }
                        None => {
                            // Hold!!
                            named = false;
                            let field_ident =
                                field_counter.to_string();
                            let field_ident = format_ident!(
                                "variant_member_{}",
                                field_ident
                            );
                            members
                                .push(field_ident.clone());
                            field.ident = Some(field_ident);
                        }
                    }
                }

                let buffer_array = self
                    .fn_buffer_array_from_fields::<false>(
                        None, &fields,
                    )?;
                let buffer_block = self
                    .buffer_block_from_array(
                        &buffer_array,
                    )?;

                let members = match named {
                    false => quote! {(#(#members),*)},
                    true => quote! {{#(#members),*}},
                };

                let expr = quote! {
                    #match_type #members  => #buffer_block
                };
                Ok(expr)
            })
            .collect::<Result<Vec<_>, Error>>()?;

        let block = quote! {{
            match self {
                #(#match_arms)*
            }
        }};
        let block = parse2::<Block>(block)?;
        Ok(block)
    }

    fn fn_buffer_array_from_fields<const REF: bool>(
        &self,
        parent_ident: Option<&Ident>,
        fields: &Fields,
    ) -> Result<ExprArray, Error> {
        // Create parent identifier
        let parent_ident = if let Some(ident) = parent_ident
        {
            let ident =
                parse2::<Expr>(ident.into_token_stream())?;
            Some(ident)
        }
        else {
            None
        };

        let fields = fields.iter().filter(|field| {
            let attributes = &field.attrs;
            !Self::ignore_token(attributes)
        });

        // Create field field statements
        let fields_stmts = fields
            .map(|Field { ident, ty, .. }| (ident, ty));
        let fields_stmts = self
            .fn_buffer_expr_from_fields::<_, _, REF>(
                parent_ident,
                |lit_int| {
                    quote! {.#lit_int}
                },
                fields_stmts,
            )?;

        let expr_array = quote! {
            [#(#fields_stmts),*]
        };
        let expr_array = parse2::<ExprArray>(expr_array)?;
        Ok(expr_array)
    }

    fn buffer_block_from_array(
        &self,
        expr_array: &ExprArray,
    ) -> Result<Block, Error> {
        let block = quote! {{
            let buffers = #expr_array.into_iter();

            buffers
                .flatten()
                .reduce(|acc, current| acc + current)
        }};
        let block = parse2::<Block>(block)?;
        Ok(block)
    }

    fn fn_buffer_expr_from_fields<
        'field,
        TypeIter,
        Transformer,
        const REF: bool,
    >(
        &self,
        parent_ident: Option<Expr>,
        transform: Transformer,
        fields: TypeIter,
    ) -> Result<Vec<Expr>, Error>
    where
        TypeIter: Iterator<
            Item = (&'field Option<Ident>, &'field Type),
        >,
        Transformer: Fn(LitInt) -> TokenStream,
    {
        let mut field_counter = 0usize;
        fields
            .map(|(ident, ty)| {
                // Create ident
                let expr_field = if let Some(parent_ident) = &parent_ident {
                    match ident {
                        Some(self_ident) => {
                            quote! {#parent_ident.#self_ident}
                        }
                        None => {
                            let field_counter = field_counter.to_string();
                            let field_counter = LitInt::new(field_counter.as_str(), ty.span());
                            let field_counter = transform(field_counter);
                            quote! {#parent_ident #field_counter}
                        }
                    }
                } else {
                    match ident {
                        Some(self_ident) => quote! {#self_ident},
                        None => panic!("logic error; this should never happen"),
                    }
                };
                let expr_field = parse2::<Expr>(expr_field)?;

                let buffer_expr_field = self.fn_buffer_expr_from_field::<REF>(&expr_field, ty)?;
                field_counter += 1;
                Ok(buffer_expr_field)
            })
            .collect()
    }

    fn fn_buffer_expr_from_field<const REF: bool>(
        &self,
        expr_field: &Expr,
        ty: &Type,
    ) -> Result<Expr, Error> {
        // Check Type
        let expr = if let Type::Path(path) = ty {
            self.fn_buffer_expr_from_typepath::<REF>(
                expr_field, path, ty,
            )
        }
        else if let Type::Array(array) = ty {
            self.fn_buffer_expr_from_typearray(
                expr_field, array,
            )
        }
        else if let Type::Tuple(tuple) = ty {
            self.fn_buffer_expr_from_typetuple(
                expr_field, tuple,
            )
        }
        else {
            self.fn_buffer_expr_from_non_terminal::<REF>(
                expr_field, ty,
            )
        }?;
        Ok(expr)
    }

    fn fn_buffer_expr_from_typepath<const REF: bool>(
        &self,
        expr_field: &Expr,
        path: &TypePath,
        ty: &Type,
    ) -> Result<Expr, Error> {
        let path_seg = path.path.segments.last().ok_or(
            Error::new(path.span(), "invalid path"),
        )?;
        // Call terminal_path only if the path is "Buffer"
        if path_seg.ident == "Buffer" {
            let reference =
                if REF { Some(quote! {&}) } else { None };
            let expr = quote! {
                Some(::std::clone::Clone::clone(#reference #expr_field))
            };
            let expr = parse2::<Expr>(expr)?;
            Ok(expr)
        }
        else {
            self.fn_buffer_expr_from_non_terminal::<REF>(
                expr_field, ty,
            )
        }
    }

    fn fn_buffer_expr_from_typearray(
        &self,
        expr_field: &Expr,
        array: &TypeArray,
    ) -> Result<Expr, Error> {
        // Get element Type
        let elem = array.elem.as_ref();

        // Get length
        let len = array.len.clone();
        let len =
            parse2::<LitInt>(len.into_token_stream())?;
        let len = len.base10_parse::<usize>()?;

        // Get range
        let fields_stmts =
            (0..len).map(|_| (&None::<Ident>, elem));

        let fields_stmts = self
            .fn_buffer_expr_from_fields::<_, _, true>(
                Some(expr_field.clone()),
                |lit_int| {
                    quote! {[#lit_int]}
                },
                fields_stmts,
            )?;

        let expr_array = quote! {
            [#(#fields_stmts),*]
        };
        let expr_array = parse2::<ExprArray>(expr_array)?;
        let expr =
            self.buffer_block_from_array(&expr_array)?;
        let expr =
            parse2::<Expr>(expr.into_token_stream())?;
        Ok(expr)
    }

    fn fn_buffer_expr_from_typetuple(
        &self,
        expr_field: &Expr,
        ty: &TypeTuple,
    ) -> Result<Expr, Error> {
        // Get range
        let fields_stmts = ty
            .elems
            .iter()
            .map(|elem| (&None::<Ident>, elem));
        let fields_stmts = self
            .fn_buffer_expr_from_fields::<_, _, true>(
                Some(expr_field.clone()),
                |lit_int| {
                    quote! {.#lit_int}
                },
                fields_stmts,
            )?;

        let expr_array = quote! {
            [#(#fields_stmts),*]
        };
        let expr_array = parse2::<ExprArray>(expr_array)?;
        let expr =
            self.buffer_block_from_array(&expr_array)?;
        let expr =
            parse2::<Expr>(expr.into_token_stream())?;
        Ok(expr)
    }

    fn fn_buffer_expr_from_non_terminal<const REF: bool>(
        &self,
        expr_field: &Expr,
        ty: &Type,
    ) -> Result<Expr, Error> {
        let module_name = &self.module_name;
        let reference =
            if REF { Some(quote! {&}) } else { None };
        let expr = quote! {
            <#ty as #module_name::Token>::buffer(#reference #expr_field)
        };
        let expr = parse2::<Expr>(expr)?;
        Ok(expr)
    }

    fn fn_lexer(&self) -> Result<ItemFn, Error> {
        // Grab lifetime
        let new_lifetime = &self.new_lifetime;

        // Create block
        let block = match &self.data {
            Data::Struct(data) => {
                // Constructor ty
                let construct_ty = quote! {Self};
                let construct_ty =
                    parse2::<Type>(construct_ty)?;

                let block = self.fn_lexer_from_fields(
                    &construct_ty,
                    &data.fields,
                )?;
                Ok::<Block, Error>(block)
            }
            Data::Enum(data) => {
                self.fn_lexer_from_enum(data)
            }
        }?;

        let module_name = &self.module_name;
        // Create function body
        let item_fn = quote! {
            fn lex(
                buffer: & mut #module_name::Buffer<#new_lifetime>
            ) -> Result<Self, #module_name::Error<#new_lifetime>> #block
        };
        let item_fn = parse2::<ItemFn>(item_fn)?;

        // Return
        Ok(item_fn)
    }

    fn fn_lexer_from_enum(
        &self,
        data: &DataEnum,
    ) -> Result<Block, Error> {
        // Destruct
        let DataEnum { variants, .. } = data;

        // Get buffer ident
        let module_name = &self.module_name;
        let buffer_ident = self.buffer_ident();
        let local_buffer_ident = &self.local_buffer_ident();
        let local_error_ident =
            format_ident!("local_error");
        let new_lifetime = &self.new_lifetime;

        // Create variants
        let mut field_counter = 0usize;
        let variants_delcs = variants
            .iter()
            .map(|variant| {
                // Destruct field
                let Variant { ident, fields, .. } = variant;

                // Create fieled counter
                let field_counter_str = field_counter.to_string();
                let field_counter_str = format_ident!("local_variant_{}", field_counter_str);

                // Create variant type
                let construct_ty = quote! {
                    Self::#ident
                };
                let construct_ty = parse2::<Type>(construct_ty)?;

                // Make body
                let block = self.fn_lexer_from_fields(&construct_ty, fields)?;

                // make closure
                let closure = quote! {
                    let #field_counter_str = |#buffer_ident: &mut #module_name::Buffer<#new_lifetime>| #block
                };
                let closure = parse2::<ExprLet>(closure)?;

                field_counter += 1;
                Ok(closure)
            })
            .collect::<Result<Vec<_>, Error>>()?;

        // Create variants
        let mut field_counter = 0usize;
        let variants_matches = variants
            .iter()
            .map(|_| {
                // Create fieled counter
                let field_counter_str = field_counter.to_string();
                let field_counter_str = format_ident!("local_variant_{}", field_counter_str);

                // Create variant type
                let match_stmt = quote! {
                    match #field_counter_str(&mut #local_buffer_ident) {
                        Ok(result) => {
                            break Ok(result);
                        }
                        Err(err) => match &#local_error_ident {
                            Some(local_err) => {
                                if err.matched >= local_err.matched {
                                    #local_error_ident = Some(err)
                                }
                            }
                            None => #local_error_ident = Some(err),
                        }
                    }
                };
                let match_stmt = parse2::<ExprMatch>(match_stmt)?;

                field_counter += 1;
                Ok(match_stmt)
            })
            .collect::<Result<Vec<_>, Error>>()?;

        // Create blocks
        let block = quote! {{
            let mut #local_buffer_ident = #buffer_ident.clone();
            let mut #local_error_ident = None::<#module_name::Error<#new_lifetime>>;

            #(#variants_delcs);*;

            let results = loop {
                #(#variants_matches)*

                break match #local_error_ident {
                    Some(err) => Err(err),
                    None => Err(#module_name::Error{
                        buffer: #buffer_ident.clone(),
                        matched: 0,
                        kind: #module_name::Kind::NotFound(<Self as #module_name::Token>::NAME)
                    })
                }
            }?;

            *#buffer_ident = #local_buffer_ident;
            Ok(results)
        }};
        let block = parse2::<Block>(block)?;

        // Return
        Ok(block)
    }

    fn fn_lexer_from_fields(
        &self,
        construct_ty: &Type,
        fields: &Fields,
    ) -> Result<Block, Error> {
        // Create count
        let count = self.count();
        let buffer_ident = self.buffer_ident();
        let local_buffer_ident = self.local_buffer_ident();
        let new_lifetime = &self.new_lifetime;

        // Check if named or not
        let named = match fields {
            Fields::Named(_) => true,
            Fields::Unnamed(_) => false,
            Fields::Unit => Err(Error::new(
                fields.span(),
                "unit fields are not supported",
            ))?,
        };

        // Check if consume before
        let before = self.eat(&self.before)?;

        // Check if consume after
        let after = self.eat(&self.after)?;

        // Iterate statements
        let fields_stmts =
            self.fn_lexer_stmt_from_fields(&count, fields)?;

        // Create field values
        let fields_values =
            self.fn_lexer_from_field(&named, fields)?;

        // Return value
        let ok_value = if named {
            quote! {
                {#(#fields_values),*}
            }
        }
        else {
            quote! {
                (#(#fields_values),*)
            }
        };

        let ok_value = quote! {
            #construct_ty #ok_value
        };

        let ok_value = if let Some(hook) = &self.lexer_hook
        {
            quote_spanned! { hook.span() =>
                #hook(#ok_value)?
            }
        }
        else {
            ok_value
        };

        let module_name = &self.module_name;
        let block = quote! {{
            let mut #local_buffer_ident = #buffer_ident.clone();
            let #count: ::std::primitive::usize = 0;

            #(#before);*;

            #(#fields_stmts);*;

            #(#after);*;

            *#buffer_ident = #local_buffer_ident;
            let ok_value = #ok_value;
            Ok::<Self, #module_name::Error<#new_lifetime>>(ok_value)
        }};

        let block = parse2::<Block>(block)?;
        Ok(block)
    }

    fn fn_lexer_from_field(
        &self,
        named: &bool,
        fields: &Fields,
    ) -> Result<Vec<FieldValue>, Error> {
        let mut field_counter = 0usize;
        let fields_values = fields
            .iter()
            .map(|field| {
                // destruct field
                let Field { ident, .. } = field;

                // Create field ident
                let field_counter_str =
                    field_counter.to_string();
                let field_counter_str = format_ident!(
                    "local_member_{}",
                    field_counter_str
                );

                // Check if named
                let field_value = if *named {
                    quote! {
                        #ident: #field_counter_str
                    }
                }
                else {
                    quote! {
                        #field_counter_str
                    }
                };
                let field_value =
                    parse2::<FieldValue>(field_value);

                field_counter += 1;
                field_value
            })
            .collect::<Result<Vec<_>, Error>>()?;
        Ok(fields_values)
    }

    fn fn_lexer_stmt_from_fields(
        &self,
        count: &Ident,
        fields: &Fields,
    ) -> Result<Vec<Expr>, Error> {
        // Iterate statements
        let mut field_counter = 0usize;
        let new_lifetime = &self.new_lifetime;
        let fields_stmts = fields
            .iter()
            .map(|field| {
                // destruct field
                let Field { attrs, ty, .. } = field;

                // Create ident
                let field_counter_str = field_counter.to_string();
                let field_counter_str = format_ident!("local_member_{}", field_counter_str);

                // Create field expr
                let expr = self.fn_lexer_expr_from_field(attrs, ty)?;

                // Create let expr
        let module_name = &self.module_name;
                let let_expr = quote! {
                    let #field_counter_str = #expr.map_err(|mut err: #module_name::Error<#new_lifetime>| {
                        err.matched += count;
                        err
                    })?
                };
                let let_expr = parse2::<ExprLet>(let_expr)?;
                let let_expr = Expr::from(let_expr);

                field_counter += 1;
                Ok(let_expr)
            })
            .intersperse({
                // Creat binary expresion
                let expr_let = quote! {
                    let #count = #count + 1
                };
                let expr_let = parse2::<ExprLet>(expr_let)?;
                Ok(expr_let.into())
            })
            .collect::<Result<Vec<_>, Error>>()?;
        Ok(fields_stmts)
    }

    fn fn_lexer_expr_from_field(
        &self,
        attributes: &[Attribute],
        ty: &Type,
    ) -> Result<Expr, Error> {
        // Make buffers
        let local_buffer_ident = self.local_buffer_ident();

        // Check Type
        let expr = if let Type::Path(path) = ty {
            self.fn_lexer_expr_from_typepath(
                attributes, path, ty,
            )
        }
        else if let Type::Array(array) = ty {
            self.fn_lexer_expr_from_typearray(array)
        }
        else if let Type::Tuple(tuple) = ty {
            self.fn_lexer_expr_from_typetuple(tuple)
        }
        else {
            self.fn_lexer_expr_from_non_terminal(
                &local_buffer_ident,
                ty,
            )
        }?;
        Ok(expr)
    }

    fn fn_lexer_expr_from_typepath(
        &self,
        attributes: &[Attribute],
        path: &TypePath,
        ty: &Type,
    ) -> Result<Expr, Error> {
        // Make buffers
        let buffer_ident = self.buffer_ident();
        let local_buffer_ident = self.local_buffer_ident();

        let path_seg = path.path.segments.last().ok_or(
            Error::new(path.span(), "invalid path"),
        )?;

        // Buffer type
        if let Some(build_with) =
            Self::build_with(attributes)
        {
            let build_with = build_with?;
            let expr = quote_spanned! {ty.span() =>
                #build_with
            };
            let expr = parse2::<Expr>(expr)?;
            Ok(expr)
        }
        else if path_seg.ident == "Buffer" {
            let lit_strs = Self::get_patterns(attributes)?;
            self.fn_lexer_expr_from_terminal(
                &buffer_ident,
                &local_buffer_ident,
                lit_strs,
            )
        }
        else {
            self.fn_lexer_expr_from_non_terminal(
                &local_buffer_ident,
                ty,
            )
        }
    }

    fn fn_lexer_expr_from_typearray(
        &self,
        array: &TypeArray,
    ) -> Result<Expr, Error> {
        // Create count
        let count = self.count();

        // Get element Type
        let elem = &array.elem.as_ref();

        // Get length
        let len = array.len.clone();
        let len =
            parse2::<LitInt>(len.into_token_stream())?;
        let len = len.base10_parse::<usize>()?;

        // do another test
        let fields = (0..len).map(|_| (*elem).clone());
        let fields = quote! {
            (#(#fields),*)
        };
        let fields = parse2::<FieldsUnnamed>(fields)?;
        let fields = Fields::from(fields);

        // Create field values
        let fields_stmts = self
            .fn_lexer_stmt_from_fields(&count, &fields)?;
        let fields_values =
            self.fn_lexer_from_field(&false, &fields)?;

        // Create block
        let block = quote! {{
            #(#fields_stmts);*;
            Ok([
                #(#fields_values),*
            ])
        }};

        // Return block
        let block = parse2::<ExprBlock>(block)?;
        Ok(block.into())
    }

    fn fn_lexer_expr_from_typetuple(
        &self,
        tuple: &TypeTuple,
    ) -> Result<Expr, Error> {
        // Create count
        let count = self.count();

        // Get exprs
        let fields = tuple.elems.iter();
        let fields = quote! {
            (#(#fields),*)
        };
        let fields = parse2::<FieldsUnnamed>(fields)?;
        let fields = Fields::from(fields);

        // Create field values
        let fields_stmts = self
            .fn_lexer_stmt_from_fields(&count, &fields)?;
        let fields_values =
            self.fn_lexer_from_field(&false, &fields)?;

        // Create block
        let block = quote! {{
            #(#fields_stmts);*;
            Ok((
                #(#fields_values),*
            ))
        }};

        // Return block
        let block = parse2::<ExprBlock>(block)?;
        Ok(block.into())
    }

    fn fn_lexer_expr_from_non_terminal(
        &self,
        local_buffer_ident: &Ident,
        ty: &Type,
    ) -> Result<Expr, Error> {
        let module_name = &self.module_name;
        let expr = quote_spanned! {ty.span() =>
            <#ty as #module_name::Lexer>::lex(&mut #local_buffer_ident)
        };
        let expr = parse2::<Expr>(expr)?;
        Ok(expr)
    }

    fn fn_lexer_expr_from_terminal(
        &self,
        buffer_ident: &Ident,
        local_buffer_ident: &Ident,
        lit_strs: Vec<LitStr>,
    ) -> Result<Expr, Error> {
        // Create iterator
        let lit_strs_iter = lit_strs.iter().map(|lit_str| {
            let expr_if = quote! {
                if let Some(token) = #local_buffer_ident.eat_word(#lit_str) {
                    Ok(token)
                }
            };

            parse2::<ExprIf>(expr_if)
        });
        let lit_strs_iter = lit_strs_iter
            .collect::<Result<Vec<_>, Error>>()?;

        let module_name = &self.module_name;

        // Check if lit_strs is empty
        let lit_strs = if lit_strs.is_empty() {
            let expr_if = quote! {
                if let Some(token) =  #local_buffer_ident.advance_char(1){
                    Ok(token)
                } else{
                    Err(#module_name::Error{
                        buffer: #buffer_ident.clone(),
                        matched: 0,
                        kind: #module_name::Kind::NotFound(<Self as #module_name::Token>::NAME)
                    })
                }
            };

            parse2::<ExprIf>(expr_if)?
        }
        else {
            let expr_if = quote! {
                #(#lit_strs_iter)else * else {
                    Err(#module_name::Error{
                        buffer: #buffer_ident.clone(),
                        matched: 0,
                        kind: #module_name::Kind::NotFound(<Self as #module_name::Token>::NAME)
                    })
                }
            };

            parse2::<ExprIf>(expr_if)?
        };

        Ok(Expr::from(lit_strs))
    }

    fn ignore_token(attributes: &[Attribute]) -> bool {
        let build_with =
            Self::build_with(attributes).is_some();

        let ignore_token = attributes
            .iter()
            .find_map(|attr| {
                if let Meta::Path(path) = &attr.meta {
                    let path = path.get_ident()?;
                    if path == "ignore_token" {
                        Some(())
                    }
                    else {
                        None
                    }
                }
                else {
                    None
                }
            })
            .is_some();

        build_with || ignore_token
    }

    fn eat(
        &self,
        types: &Option<Vec<Type>>,
    ) -> Result<Vec<Expr>, Error> {
        let local_buffer_ident = self.local_buffer_ident();
        let new_lifetime = &self.new_lifetime;
        let module_name = &self.module_name;

        let mut exprs = Vec::new();

        if let Some(types) = types {
            for ty in types {
                let expr = self
                    .fn_lexer_expr_from_non_terminal(
                        &local_buffer_ident,
                        ty,
                    )?;

                let expr = quote! {
                    let _discard = #expr.map_err(|mut err: #module_name::Error<#new_lifetime> | {
                        err.matched += count;
                        err
                    })?
                };

                let expr = parse2::<Expr>(expr)?;

                exprs.push(expr)
            }
        }

        Ok(exprs)
    }

    fn build_with(
        attributes: &[Attribute],
    ) -> Option<Result<Expr, Error>> {
        attributes
            .iter()
            .find_map(|attr| {
                if let Meta::List(list) = &attr.meta {
                    let path = list.path.get_ident()?;
                    if path == "build_with" {
                        Some(list.tokens.clone())
                    }
                    else {
                        None
                    }
                }
                else {
                    None
                }
            })
            .map(parse2::<Expr>)
    }

    fn get_patterns(
        attributes: &[Attribute],
    ) -> Result<Vec<LitStr>, Error> {
        let mut lit_strs = Vec::new();

        // Filter "pattern" statements
        let attributes =
            attributes.iter().filter_map(|attribute| {
                let path = attribute.path();
                let path = path.segments.last()?;
                let name = &path.ident;
                if *name == "pattern" {
                    Some(&attribute.meta)
                }
                else {
                    None
                }
            });

        // Walk through pattern statements
        for attribute in attributes {
            match attribute {
                // Paths are useles and rejected
                syn::Meta::Path(path) => Err(Error::new(
                    path.span(),
                    "pattern attribute must contain values",
                ))?,
                // List e.g. [pattern("Hello", "bye")]
                syn::Meta::List(list) => {
                    Self::patterns_list(
                        &mut lit_strs,
                        list.tokens.clone(),
                    )?
                }
                // Name value: [pattern = "what"]
                syn::Meta::NameValue(name_value) => {
                    Self::patterns_named(
                        &mut lit_strs,
                        &name_value.value,
                    )?
                }
            }
        }

        Ok(lit_strs)
    }

    fn patterns_list(
        lit_strs: &mut Vec<LitStr>,
        list: TokenStream,
    ) -> Result<(), Error> {
        let pattern = punctuated::Punctuated::<
            LitStr,
            Token![,],
        >::parse_terminated;
        let lit_str_append = pattern.parse2(list)?;
        let mut lit_str_append = lit_str_append
            .iter()
            .cloned()
            .collect::<Vec<_>>();
        lit_strs.append(&mut lit_str_append);
        Ok(())
    }

    fn patterns_named(
        lit_strs: &mut Vec<LitStr>,
        lit_str: &Expr,
    ) -> Result<(), Error> {
        // Extract literal expression
        let lit_str = if let Expr::Lit(lit) = &lit_str {
            lit
        }
        else {
            Err(Error::new(
                lit_str.span(),
                "expecting literal expression",
            ))?
        };
        // Extract liteal string
        let lit_str =
            if let Lit::Str(lit_str) = &lit_str.lit {
                lit_str
            }
            else {
                Err(Error::new(
                    lit_str.span(),
                    "expecting literal string",
                ))?
            };

        // Clone lit string
        let lit_str = lit_str.clone();

        lit_strs.push(lit_str);
        Ok(())
    }

    fn local_buffer_ident(&self) -> Ident {
        Self::localise_ident(self.buffer_ident())
    }

    fn localise_ident(ident: Ident) -> Ident {
        format_ident!("local_{}", ident)
    }

    fn count(&self) -> Ident {
        format_ident!("count")
    }

    fn buffer_ident(&self) -> Ident {
        format_ident!("buffer")
    }

    fn extract_attr<'attr, Attrs>(
        attrs: Attrs,
        name: &str,
    ) -> Option<TokenStream>
    where
        Attrs: Iterator<Item = &'attr Attribute>,
    {
        attrs
            .filter_map(|attr| {
                if let Meta::List(attr) = &attr.meta {
                    Some(attr)
                }
                else {
                    None
                }
            })
            .find_map(|list| {
                let ident = list.path.get_ident()?;
                if ident == name {
                    let expr = list.tokens.clone();
                    Some(expr)
                }
                else {
                    None
                }
            })
    }
}

impl TryFrom<TokenStream> for LexerImpl {
    type Error = Error;

    fn try_from(
        expr: TokenStream,
    ) -> Result<Self, Self::Error> {
        // Create expression
        let DeriveInput {
            ident,
            generics,
            data,
            attrs,
            ..
        } = parse2::<DeriveInput>(expr)?;

        let punc_parser = |input: ParseStream| {
            Punctuated::<Type, Token![,]>::parse_terminated(
                input,
            )
        };

        let before = match Self::extract_attr(
            attrs.iter(),
            "before",
        ) {
            Some(tokens) => {
                let types = punc_parser.parse2(tokens)?;
                let types =
                    types.into_iter().collect::<Vec<_>>();
                Some(types)
            }
            None => None,
        };

        let after =
            match Self::extract_attr(attrs.iter(), "after")
            {
                Some(tokens) => {
                    let types =
                        punc_parser.parse2(tokens)?;
                    let types = types
                        .into_iter()
                        .collect::<Vec<_>>();
                    Some(types)
                }
                None => None,
            };

        let lexer_hook = match Self::extract_attr(
            attrs.iter(),
            "hook",
        ) {
            Some(tokens) => Some(parse2::<Expr>(tokens)?),
            None => None,
        };

        let module_name = attrs
            .iter()
            .filter_map(|attr| {
                if let Meta::NameValue(name_value) =
                    &attr.meta
                {
                    Some(name_value)
                }
                else {
                    None
                }
            })
            .find_map(|name_value| {
                let name = name_value.path.get_ident()?;
                if name == "lexerus" {
                    Some(name_value.value.clone())
                }
                else {
                    None
                }
            });
        let module_name = match module_name {
            Some(ident) => {
                // Obtain ident as str
                let ident = parse2::<LitStr>(
                    ident.into_token_stream(),
                )?;

                // Parse value
                ident.value()
            }
            None => ::std::env::var("LEXERUS_PKG_NAME")
                .unwrap_or("lexerus".into()),
        };
        let module_name = format_ident!("{module_name}");

        let data = match data {
            syn::Data::Struct(data) => {
                Ok(Data::Struct(data))
            }
            syn::Data::Enum(data) => Ok(Data::Enum(data)),
            syn::Data::Union(data) => Err(Error::new(
                data.union_token.span(),
                "union types not permitted",
            )),
        }?;

        // Clone
        let mut new_generics = generics.clone();

        // Fetch span
        let new_lifetime_span = new_generics.span();

        // Create new lifetime
        let new_lifetime = new_generics.lifetimes().fold(
            String::from("'new_"),
            |mut new_lifetime, param| {
                let lifetime =
                    param.lifetime.ident.to_string();
                new_lifetime.push_str(&lifetime);
                new_lifetime
            },
        );
        let new_lifetime =
            Lifetime::new(&new_lifetime, new_lifetime_span);

        // Create new lifetime param
        let new_lifetime_param = {
            let existing_lifetimes =
                new_generics.lifetimes();
            let existing_lifetimes = existing_lifetimes
                .map(|param| param.lifetime.clone());

            parse2::<GenericParam>(quote! {
                #new_lifetime: #(#existing_lifetimes)+*
            })
            .unwrap()
        };

        // Push new lifetime onto existing lifetimes
        for param in new_generics.lifetimes_mut() {
            param.bounds.push(new_lifetime.clone());
        }

        // Add new generic lifetime
        new_generics.params.push(new_lifetime_param);

        Ok(Self {
            module_name,
            lexer_hook,
            ident,
            new_lifetime,
            new_generics,
            generics,
            data,
            before,
            after,
        })
    }
}

impl From<LexerImpl> for TokenStream {
    fn from(impl_bnf: LexerImpl) -> Self {
        // Create impl bnf
        let impl_bnf = match impl_bnf.impl_trait() {
            Ok(ok) => ok,
            Err(err) => err.into_compile_error(),
        };

        quote! {
            #impl_bnf
        }
    }
}

#[cfg(test)]
mod tests;
