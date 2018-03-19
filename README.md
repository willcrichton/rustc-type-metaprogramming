# Type-directed metaprogramming in Rust

Materials accompanying the note ["Type-directed metaprogramming in Rust."](http://willcrichton.net/notes/type-directed-metaprogramming-in-rust/) Each subdirectory is its own crate, and will be need to be run in nightly. For example:

```
$ cd run-compiler
$ rustup override set nightly-2018-03-19
$ cargo run
```

The order is the same in the blog post: `run-compiler`, `extract-types`, and `auto-gc`.
