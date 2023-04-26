use egg::{rewrite as rw, *};






fn main() {
    let expr: RecExpr<LinDiff4ws> = "(+ (- (+ (- (* 6 j)) (* 1 j))) (+ (* 4 h) (* 5 g)))".parse().unwrap();
    // let expr: RecExpr<LinDiff> = "(+ (+ x z) (+ w (- x)))".parse().unwrap();
    // let expr: RecExpr<LinDiff> = "(- (+ x (- x)))".parse().unwrap();
    let r = rules4w();
    let runner = Runner::default().with_expr(&expr).run(&r);

    let extractor = Extractor::new(&runner.egraph, LinDiff4wCostFn);
    // let extractor = Extractor::new(&runner.egraph, AstSize);
    let (best_cost, best_expr) = extractor.find_best(runner.roots[0]);

    println!("Cost: {}", best_cost);
    println!("Expr: {}", best_expr);
   // runner.egraph.dot().to_pdf("graph.pdf").unwrap();
}
