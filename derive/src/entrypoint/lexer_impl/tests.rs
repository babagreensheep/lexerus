use super::*;

#[test]
fn unit_struct() {
    let expr = quote! {
        struct Meh<'code>(#[pattern("hello", "world")] Buffer<'code>, Buffer<'code>, Char<'code>);
    };

    println!("{expr}");
    let parse_impl = LexerImpl::try_from(expr).unwrap();
    let parse_impl = parse_impl.impl_trait().unwrap();
    println!("{}", parse_impl.into_token_stream());
}

#[test]
fn unit_enum() {
    let expr = quote! {
        enum EnumTester<'code> {
            One(Buffer<'code>),
            V1(Tester<'code>, [Tester<'code>; 2]),
            V2 { member: Tester<'code> },
        }
    };

    println!("{expr}");
    let parse_impl = LexerImpl::try_from(expr).unwrap();
    let parse_impl = parse_impl.impl_trait().unwrap();
    println!("{}", parse_impl.into_token_stream());
}

#[test]
fn array_struct() {
    let expr = quote! {
        struct Array<'code> {
            one: [Tester<'code>; 2],
        }
    };

    println!("{expr}");
    let parse_impl = LexerImpl::try_from(expr).unwrap();
    let parse_impl = parse_impl.impl_trait().unwrap();
    println!("{}", parse_impl.into_token_stream());
}

#[test]
fn simple_array() {
    let expr = quote! {
        struct SimpleArray<'code>([Tester<'code>; 2]);
    };

    println!("{expr}");
    let parse_impl = LexerImpl::try_from(expr).unwrap();
    let parse_impl = parse_impl.impl_trait().unwrap();
    println!("{}", parse_impl.into_token_stream());
}

#[test]
fn nested_array_struct() {
    let expr = quote! {
        struct SimpleArray<'code>([[Tester<'code>; 2];2]);
    };

    println!("{expr}");
    let parse_impl = LexerImpl::try_from(expr).unwrap();
    let parse_impl = parse_impl.impl_trait().unwrap();
    println!("{}", parse_impl.into_token_stream());
}

#[test]
fn simple_tuple() {
    let expr = quote! {
        struct Tuple<'code> {
            one: (Tester<'code>, Tester<'code>, Tester<'code>),
        }
    };

    println!("{expr}");
    let parse_impl = LexerImpl::try_from(expr).unwrap();
    let parse_impl = parse_impl.impl_trait().unwrap();
    println!("{}", parse_impl.into_token_stream());
}

#[test]
fn nested_tuple() {
    let expr = quote! {
        struct Tuple<'code> {
            one: ([Tester<'code>; 2], Tester<'code>, Tester<'code>),
        }
    };

    println!("{expr}");
    let parse_impl = LexerImpl::try_from(expr).unwrap();
    let parse_impl = parse_impl.impl_trait().unwrap();
    println!("{}", parse_impl.into_token_stream());
}

#[test]
fn named_struct() {
    let expr = quote! {
        struct Tester2<'code>{
            #[pattern("hello", "world")]
            one: Buffer<'code>,
            two: Buffer<'code>,
            three: Tester<'code>
        }
    };
    println!("{expr}");
    let parse_impl = LexerImpl::try_from(expr).unwrap();
    let parse_impl = parse_impl.impl_trait().unwrap();
    println!("{}", parse_impl.into_token_stream());
}

#[test]
fn multiple_lifetimes() {
    let expr = quote! {
        struct Tester<'code, 'code>(#[pattern("hello", "world")] Buffer<'code>, Buffer<'code2>, Tester<'code>);
    };

    let parse_impl = LexerImpl::try_from(expr).unwrap();
    let parse_impl =
        parse_impl.impl_trait().map(|_| ()).unwrap_err();
    assert_eq!(
        "only one lifetime is permitted as the tokens are \
         constructed from a single source tree",
        parse_impl.to_string()
    )
}

#[test]
fn custom_pkg_name() {
    let expr = quote! {
        #[lexerus = "crate"]
        struct Meh<'code>(#[pattern("hello", "world")] Buffer<'code>, Buffer<'code>, Char<'code>);
    };

    println!("{expr}");
    let parse_impl = LexerImpl::try_from(expr).unwrap();
    let parse_impl = parse_impl.impl_trait().unwrap();
    println!("{}", parse_impl.into_token_stream());
}
