use egg::*;
use std::io;
use std::io::Read;

mod rules4;
mod rules2;

// use crate::rules2::*;
use crate::rules4::*;

fn main() {
    let r: Vec<Rewrite<LinDiff4X, ()>>  = [
        rules4_core(),
        rules4_shortcuts(),
        rules4_elimination(),
        rules4_integrate(),
        rules4_weights(),
        rules4_weight_combine()
    ].concat();
    // println!("Examples:\n{}\n{}\n{}",
    //          "(+ (- (+ (- (* 6 j)) (* 1 j))) (+ (* 4 h) (* 5 g)))",
    //          "(+ (+ x z) (+ w (- x)))",
    //          "(- (+ x (- x)))");

    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    println!("Input: {}", input);
    let expr: RecExpr<LinDiff4X> = input.parse().unwrap();
    let runner = Runner::default().with_expr(&expr).run(&r);

    let extractor = Extractor::new(&runner.egraph, LinDiff4XCostFn);
    // let extractor = Extractor::new(&runner.egraph, AstSize);
    let (best_cost, best_expr) = extractor.find_best(runner.roots[0]);
    println!("Cost: {}", best_cost);
    println!("Expr: {}", best_expr);
}
