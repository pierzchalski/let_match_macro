#![feature(proc_macro)]

#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;
extern crate proc_macro;
extern crate proc_macro2;

use proc_macro::TokenStream;

use syn::synom::Synom;
use syn::{Pat, Expr, Arm, PatIdent};

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

impl Synom for Body {
    named!(parse -> Self, alt!(
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
    ));
}

impl Synom for LetMatch {
    named!(parse -> Self, do_parse!(
        keyword!(let) >>
        pat: syn!(Pat) >>
        punct!(=) >>
        expr: syn!(Expr) >>
        keyword!(else) >>
        body: syn!(Body) >>
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
/// ```
/// #![feature(proc_macro, never_type)]
/// 
/// extern crate let_match_macro;
/// use let_match_macro::m;
/// # fn main() {}
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
///
/// ```hidden
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
/// m!(let B {mut x, mut y} = e else match {
///     other => panic!(format!("Nah, not dealing with this: {:#?}", other)),
/// });
///
/// // Asserting that x and y have the expected mutability:
/// x.push_str(", world!");
/// y = 6;
///
/// // Asserting that x and y have the expected types:
/// let _x: String = x;
/// let _y: u32 = y;
/// # }
#[proc_macro]
pub fn m(input: TokenStream) -> TokenStream {
    let LetMatch {
        pat, expr, body 
    } = syn::parse(input).unwrap();

    let (tuple_expr, tuple_pat) = pat_to_tuple_bindings(&pat);
    let matches = body_to_matches(body);

    let result = quote!(
        let #tuple_pat = match #expr {
            #pat => #tuple_expr,
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

/// Turns a pattern into a tuple of bidings
/// as well as a pattern to match against that tuple.
/// Preserves mutability, that is the following pattern:
///
/// ```ignore
/// S { ref mut x, mut y }
/// ```
///
/// Produces a tuple expression `(x, y)` and a tuple pattern
/// `(x, mut y)`. This allows matching the tuple against the
/// tuple pattern to return the same types and mutability as
/// the bindings in the original pattern.
fn pat_to_tuple_bindings(pat: &Pat) -> (Expr, Pat) {
    let bindings = pat_bindings(pat);
    let mut expr = Vec::new();
    let mut pat = Vec::new();

    for PatIdent { by_ref, mutability, ident, .. } in bindings.into_iter() {
        expr.push(ident.clone());
        pat.push(match by_ref {
            Some(_) => PatIdent {
                ident, by_ref: None, mutability: None, subpat: None,
            },
            None => PatIdent {
                mutability, ident, by_ref: None, subpat: None,
            },
        });
    };

    let expr = parse_quote!((#(#expr,)*));
    let pat = parse_quote!((#(#pat,)*));
    (expr, pat)
}

fn pat_bindings(pat: &Pat) -> Vec<PatIdent> {
    fn tuple_bindings(pat: &syn::PatTuple) -> Vec<PatIdent> {
        let front = pat.front.iter()
            .flat_map(|pat| pat_bindings(&pat));
        let back = pat.back.iter()
            .flat_map(|pat| pat_bindings(&pat));
        front.chain(back).collect()
    }
    match *pat {
        Pat::Ident(ref pat) => {
            let mut binding = pat.clone();
            binding.subpat = None;
            let mut bindings = vec![binding];
            let subpat_bindings = pat.subpat.as_ref()
                .map(|&(_ampersand, ref subpat)| pat_bindings(&subpat))
                .unwrap_or_else(Vec::new);
            bindings.extend(subpat_bindings);
            bindings
        },
        Pat::Struct(ref pat) => {
            pat.fields.iter()
                .flat_map(|field| pat_bindings(&field.pat))
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
