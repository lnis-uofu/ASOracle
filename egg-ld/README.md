# Rewriting rules

Implementation of rewriting rules using equality saturation. The rules are implemented using the [egg](https://docs.rs/egg/latest/egg/index.html) library. Rewriting rules for 2 input and 4 input summing amplifier cells are located in rules2.rs and rules4.rs respectively.

The project uses Rust and the Cargo build tool. For instructions on Rust see the [documentation](https://www.rust-lang.org/learn/get-started)

Unit tests can be run with `cargo test`

The main.rs file generates a simple executable which reads an expression in from standard input and prints out the rewritten expression and area cost using the 4 input cell rules.

```
echo "(+ (- (+ (- (* 6 j)) (* 1 j))) (+ (* 4 h) (* 5 g)))" | cargo run
```

Expressions are specified as Lisp style s-expressions. The basic operators are the binary operators add (+) and multiply (x), and unary operators negate (-) and integrate (I). Symbols and scalars can be used. The target operators are the inverting summing ($), inverting integrating (i), and summing weights (*).
