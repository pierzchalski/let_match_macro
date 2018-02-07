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
    matches: Vec<Arm>,
}

impl Synom for LetMatch {
    named!(parse -> Self, do_parse!(
        keyword!(let) >>
        pat: syn!(Pat) >>
        punct!(=) >>
        expr: syn!(Expr) >>
        keyword!(else) >>
        keyword!(match) >>
        matches: map!(
            braces!(many0!(syn!(Arm))),
            |(_parens, arms)| arms.into_iter().collect()
        ) >>
        (LetMatch {
            pat, expr, matches
        })
    ));
}

#[proc_macro]
pub fn m(input: TokenStream) -> TokenStream {
    let LetMatch {
        pat, expr, matches
    } = syn::parse(input).unwrap();

    let bindings = refutable_to_irrefutable(&pat);

    let result = quote!(
        let #bindings = match #expr {
            #pat => #bindings,
            #(#matches)*
        };
    );

    result.into()
}

// Convert refutable patterns like the following:
//
// Ok(x), T {a, b: c}
//
// Into irrefutable patterns like
//
// (x), (a, c)
//
// Thankfully, we don't need to actually preserve ordering since
// we just use the same bindings twice.
fn refutable_to_irrefutable(pat: &Pat) -> Pat {
    let idents = bindings(&pat);
    let pat = quote!( (#(#idents,)*) );
    syn::parse(pat.into()).unwrap()
}

// Collects variable bindings in patterns. For example:
//
// Struct { a, b: ref mut c } => a, c
//
// Thankfully, we don't need to actually preserve ordering since
// we just use the same bindings twice.
fn bindings(pat: &Pat) -> Vec<Ident> {
    fn tuple_bindings(pat: &syn::PatTuple) -> Vec<Ident> {
        pat.front.iter()
            .flat_map(|pat| bindings(&pat))
            .chain(pat.back.iter()
                   .flat_map(|pat| bindings(&pat)))
            .collect()
    }
    match *pat {
        Pat::Ident(ref pat) => {
            let mut idents = pat.subpat.as_ref()
                .map(|&(_ampersand, ref subpat)| bindings(&subpat))
                .unwrap_or_else(Vec::new);
            idents.push(pat.ident);
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
