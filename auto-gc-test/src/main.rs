#![feature(proc_macro)]

extern crate auto_gc;
use auto_gc::auto_gc;

fn main() {
    let x = auto_gc!{
        let y = 1 + 2;
        y + 3
    };
    //println!("{:?}", *x);
}
