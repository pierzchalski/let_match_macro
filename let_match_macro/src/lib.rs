#![feature(proc_macro)]

#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;
extern crate proc_macro;
extern crate proc_macro2;

use proc_macro::TokenStream;

use syn::synom::Synom;
use syn::{Pat, Expr, Arm, Ident};

#[derive(Debug)]
struct LetMatch {
    pat: Pat,
    expr: Expr,
    body: Body,
}

#[derive(Debug)]
enum Body {
    Arms(Vec<Arm>),
    Expr(Expr),
}

impl Synom for LetMatch {
    named!(parse -> Self, do_parse!(
        keyword!(let) >>
        pat: syn!(Pat) >>
        punct!(=) >>
        expr: syn!(Expr) >>
        keyword!(else) >>
        body: alt!(
            syn!(Expr) => { Body::Expr }
            |
            do_parse!(
                keyword!(match) >>
                arms: map!(
                    braces!(many0!(syn!(Arm))),
                    |(_parens, arms)| arms.into_iter().collect()
                ) >>
                (Body::Arms(arms))
            )
        ) >>
        (LetMatch {
            pat, expr, body
        })
    ));
}

/// Allow 'let unless' syntax:
/// 
/// ```ignore
/// let <pat: PAT> = <expr: EXPR> else <body: EXPR>;
///
/// // Expands into:
/// let pat = match expr {
///     pat => pat,
///     _ => {let _: ! = body },
/// };
/// ```
///
/// ```ignore
/// let <pat: PAT> = <expr: EXPR> else match {
///     (<match_pat: PAT> => <match_body: EXPR>),*
/// };
/// 
/// // Expands into:
/// let pat = match expr {
///     pat => pat,
///     (match_pat => { let _: ! = match_body }),*
/// }
///
/// ```
///
/// Notice that the failing body/non-successful match cases are
/// required to be divergent.
///
/// # Requirements
///
/// This is provided as a proc macro, and it uses the `!` type internally.
/// This means you need to add the following to any crates using `m!`:
///
/// ```ignore
/// #![feature(proc_macro, never_type)]
/// 
/// extern crate let_match_macro;
/// use let_match_macro::m;
/// ```
///
/// # Examples
/// 
/// We'll be using the following enum to show things off:
///
/// ```
/// #[derive(Debug)]
/// enum Example {
///     A(String),
///     B {
///         x: String,
///         y: u32,
///     },
///     C(u32, u32),
/// }
/// ```
///
/// Fairly complicated match patterns are supported:
///
/// ```
/// # #![feature(proc_macro, never_type)]
/// #
/// # extern crate let_match_macro;
/// # use let_match_macro::m;
/// #
/// # #[derive(Debug)]
/// # enum Example {
/// #     A(String),
/// #     B {
/// #         x: String,
/// #         y: u32,
/// #     },
/// #     C(u32, u32),
/// # }
/// # use self::Example::*;
/// #
/// # fn main() {
/// let mut e = B { x: "Hello".into(), y: 5 };
///
/// m!(let A(ref mut x) = e else match {
///     b @ B { .. } => {
///         println!("handling B case: {:#?}", b);
///         return;
///     },
///     C(x, y) => {
///         panic!("need to diverge...");
///     },
/// });
///
/// x.push_str(" world!");
/// assert_eq!(*x, "Hello world!".to_string());
/// # }
/// ```
///
/// You don't need to pattern-match to handle match failure:
///
/// ```should_panic
/// # #![feature(proc_macro, never_type)]
/// #
/// # extern crate let_match_macro;
/// # use let_match_macro::m;
/// #
/// # #[derive(Debug)]
/// # enum Example {
/// #     A(String),
/// #     B {
/// #         x: String,
/// #         y: u32,
/// #     },
/// #     C(u32, u32),
/// # }
/// # use self::Example::*;
/// #
/// # fn main() {
/// let mut e = B { x: "Hello".into(), y: 5 };
///
/// m!(let A(x) = e else {
///     panic!("Nah, not dealing with this");
/// });
/// # }
/// ```
///
/// This will fail because not all of the match arms diverge:
///
/// ```compile_fail
/// # #![feature(proc_macro, never_type)]
/// #
/// # extern crate let_match_macro;
/// # use let_match_macro::m;
/// #
/// # #[derive(Debug)]
/// # enum Example {
/// #     A(String),
/// #     B {
/// #         x: String,
/// #         y: u32,
/// #     },
/// #     C(u32, u32),
/// # }
/// # use self::Example::*;
/// #
/// # fn main() {
/// let mut e = B { x: "Hello".into(), y: 5 };
///
/// m!(let C(x, _) = e else match {
///     A(msg) => {
///         println!("handling A case: {}", msg);
///         // x is of type u32, but we don't let
///         // other branches 'succeed': every branch
///         // here must diverge.
///         0u32
///     },
///     b @ B { .. } => {
///         println!("handling B case: {:#?}", b);
///         return;
///     },
/// });
/// # }
///
/// ```
#[proc_macro]
pub fn m(input: TokenStream) -> TokenStream {
    let LetMatch {
        pat, expr, body 
    } = syn::parse(input).unwrap();

    let idents_tuple = pat_to_bindings(&pat);
    let matches = body_to_matches(body);

    let result = quote!(
        let #idents_tuple = match #expr {
            #pat => #idents_tuple,
            #(#matches)*
        };
    );

    result.into()
}

fn body_to_matches(body: Body) -> Vec<Arm> {
    match body {
        Body::Arms(arms) =>
            arms.into_iter().map(|mut arm| {
                let body = arm.body;
                arm.body = Box::new(
                    parse_quote!(
                        {let _: ! = #body;}
                    )
                );
                arm
            }).collect(),
        Body::Expr(expr) => 
            vec!(
                parse_quote!(
                    _ => {let _: ! = #expr;}
                )
            ),
    }
}

fn pat_to_bindings(pat: &Pat) -> Pat {
    let bindings = bindings(pat);
    parse_quote!((#(#bindings,)*))
}

fn bindings(pat: &Pat) -> Vec<Ident> {
    fn tuple_bindings(pat: &syn::PatTuple) -> Vec<Ident> {
        let front = pat.front.iter()
            .flat_map(|pat| bindings(&pat));
        let back = pat.back.iter()
            .flat_map(|pat| bindings(&pat));
        front.chain(back).collect()
    }
    match *pat {
        Pat::Ident(ref pat) => {
            let mut idents = vec![pat.ident.clone()];
            let subpat_idents = pat.subpat.as_ref()
                .map(|&(_ampersand, ref subpat)| bindings(&subpat))
                .unwrap_or_else(Vec::new);
            idents.extend(subpat_idents);
            idents
        },
        Pat::Struct(ref pat) => {
            pat.fields.iter()
                .flat_map(|field| bindings(&field.pat))
                .collect()
        },
        Pat::TupleStruct(ref pat) => {
            tuple_bindings(&pat.pat)
        },
        Pat::Tuple(ref pat) => {
            tuple_bindings(pat)
        },
        ref other => panic!(format!("Unexpected pattern {:#?}", other)),
    }
}
