// use egg::*;
// use std::io;
// use std::io::Read;
// use std::collections::HashMap;

// //mod rules4;
// mod rules2;

// use crate::rules2::*;
// //use crate::rules4::*;
fn main() {
}
//     // let r: Vec<Rewrite<LinDiff4X, ()>>  = [
//     //     rules4_core(),
//     //     rules4_shortcuts(),
//     //     rules4_elimination(),
//     //     rules4_integrate(),
//     //     rules4_weights(),
//     //     rules4_weight_combine()
//     // ].concat();
//     // println!("Examples:\n{}\n{}\n{}",
//     //          "(+ (- (+ (- (* 6 j)) (* 1 j))) (+ (* 4 h) (* 5 g)))",
//     //          "(+ (+ x z) (+ w (- x)))",
//     //          "(- (+ x (- x)))");
// /*    let r: Vec<Rewrite<LinDiff2Synth, ()>>  = [
//         rules2_core(),
//         rules2_shortcuts(),
//         rules2_elimination(),
//         rules2_integrate(),
//         rules2_weights(),
//         rules2_weight_combine()
//     ].concat();
// */
//     //    let mut input = String::new();
// //    io::stdin().read_to_string(&mut input).unwrap();
//     //    println!("Input: {}", input);
//     let mut egraph = EGraph::<LinDiff2Synth, ()>::default();
// /*
//     // Todo input
//     let outputs = Vec::new();
//     let assigns = HashMap::new();

//     let exprs:: HashMap<String, RecExpr<LinDiff2Synth>> = assigns.map_key(
//         |assign| assign.parse::<RecExpr<LinDiff2Synth>>().unwrap()
//     );
//     let mut assign_ids = HashMap<String, Id>::new()
//     let (id, expr) in &exprs {
//         let eggid = egraph.add_expr(expr);
//         assign_ids[id] = eggid;
//     }
//     let roots = outputs.map(|id| assign_ids[id]);


//     let runner = Runner::default().with_expr(&expr).run(&r);

//     let extractor = Extractor::new(&runner.egraph, LinDiff2SynthCostFn);
//     // let extractor = Extractor::new(&runner.egraph, AstSize);
//     let (best_cost, best_expr) = extractor.find_best(runner.roots[0]);
//     println!("Cost: {}", best_cost);
//     println!("Expr: {}", best_expr);
// */
// }
