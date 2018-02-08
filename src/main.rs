#![feature(proc_macro, never_type)]

extern crate let_match_macro;

use let_match_macro::m;

#[derive(Debug)]
enum E {
    A(u32),
    B {
        x: u32,
        y: String,
    }
}

fn main() {
    let mut e = E::B {
        x: 42,
        y: "hello".into(),
    };

    m!(let E::B { x, y: ref mut z } = e else match {
        E::A(x) if x > 1 => {
            println!("big x: {}", x);
            return;
        },
        a @ E::A(_) => {
            println!("something else: {:#?}", a);
            return;
        },
    });

    *z = z.to_uppercase();
    println!("x: {}. z: {}", x, z);
}
