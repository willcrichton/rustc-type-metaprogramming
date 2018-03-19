#![feature(proc_macro)]

extern crate auto_gc;
use auto_gc::auto_gc;
use std::rc::Rc;

fn main() {
    auto_gc! {
        let x = 1;
        let y = 1 + x;
    };
    println!("{:?}", *y);
}
