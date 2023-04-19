use egg::{rewrite as rw, *};

pub type EGraph4 = egg::EGraph<LinDiff4, ()>;
pub type Rewrite4 = egg::Rewrite<LinDiff4, ()>;

pub type EGraph = egg::EGraph<LinDiff, ()>;
pub type Rewrite = egg::Rewrite<LinDiff, ()>;

pub type EGraph0 = egg::EGraph<LinDiff0, ()>;
pub type Rewrite0 = egg::Rewrite<LinDiff0, ()>;


define_language! {
    pub enum LinDiff0 {
        // Non-inverting versions
        "+" = Add([Id; 2]),
        "i" = Integrate(Id),
        // Inverting amp versions
        "$" = Summing([Id; 4]),
        "I" = Integrating(Id),
        // Negative is effectively summing with one input
        "-" = Negative(Id),
        Constant(i32),
        Symbol(Symbol),
    }
}

define_language! {
    pub enum LinDiff4 {
        "+" = Add([Id; 2]),
        "$2" = Summing2([Id; 2]),
        "$3" = Summing3([Id; 3]),
        "$4" = Summing4([Id; 4]),
//        "i" = Integrate(Id),
        "-" = Negative(Id),
        Constant(i32),
        Symbol(Symbol),
    }
}

define_language! {
    pub enum LinDiff {
        "+" = Add([Id; 2]),
        "$" = Summing([Id; 2]),
        "-" = Negative(Id),
        Constant(i32),
        Symbol(Symbol),
    }
}


pub struct LinDiffCostFn;
impl egg::CostFunction<LinDiff> for LinDiffCostFn {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &LinDiff, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            LinDiff::Add(..) => 2,
            LinDiff::Summing(..) => 1,
            LinDiff::Symbol(..) => 0,
            _ => 1,
        };
        enode.fold(op_cost, |sum, i| sum + costs(i))
    }
}


pub struct LinDiff4CostFn;
impl egg::CostFunction<LinDiff4> for LinDiff4CostFn {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &LinDiff4, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            LinDiff4::Add(..) => 2,
            LinDiff4::Symbol(..) => 0,
            _ => 1,
        };
        enode.fold(op_cost, |sum, i| sum + costs(i))
    }
}

pub struct LinDiff0CostFn;
impl egg::CostFunction<LinDiff0> for LinDiff0CostFn {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &LinDiff0, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            LinDiff0::Add(..) => 2,
            LinDiff0::Integrate(..) => 2,
            LinDiff0::Symbol(..) => 0,
            _ => 1,
        };
        enode.fold(op_cost, |sum, i| sum + costs(i))
    }
}

pub fn rules0() -> Vec<Rewrite0> { vec![
        rw!("eliminate-add"; "(+ ?a ?b)" => "(- ($ ?a ?b 0 0))"),
        rw!("eliminate-integral"; "(i ?a)" => "(- (I ?a))"),

        rw!("integrate-split"; "(I ($ ?a ?b ?c ?d))" => "($ (I $?a) (I ?b) (I ?c) (I ?d))"),
        rw!("integrate-join"; "($ (I $?a) (I ?b) (I ?c) (I ?d))" => "(I ($ ?a ?b ?c ?d))"),

        rw!("invert-sum"; "(- ?a)" => "($ ?a 0 0 0)"),
        rw!("sum-invert"; "($ ?a 0 0 0)" => "(- ?a)"),

        // Negation
        rw!("factor-neg"; "($ (- ?a) (- ?b) (- ?c) (- ?d))" => "(- ($ ?a ?b ?c ?d))"),
        rw!("distrib-neg"; "(- ($ ?a ?b ?c ?d))" => "($ (- ?a) (- ?b) (- ?c) (- ?d))"),
        rw!("integrate-0"; "(I 0)" => "0"),
        rw!("0-neg"; "0" => "(- 0)"),
        rw!("neg-0"; "(- 0)" => "0"),
        rw!("double-neg"; "(- (- ?a))" => "?a"),
        rw!("neg-double"; "?a" => "(- (- ?a))"),

        rw!("cancel-add"; "($ (- ?a) ?a ?b ?c)" => "($ ?b ?c 0 0)"),

        // Commutative properties
        rw!("commute-sum-1"; "($ ?a ?b ?c ?d)" => "($ ?a ?b ?d ?c)"),
        rw!("commute-sum-2"; "($ ?a ?b ?c ?d)" => "($ ?a ?c ?b ?d)"),
        rw!("commute-sum-3"; "($ ?a ?b ?c ?d)" => "($ ?a ?c ?d ?b)"),
        rw!("commute-sum-4"; "($ ?a ?b ?c ?d)" => "($ ?a ?d ?b ?c)"),
        rw!("commute-sum-5"; "($ ?a ?b ?c ?d)" => "($ ?a ?d ?c ?b)"),

        rw!("commute-sum-6"; "($ ?a ?b ?c ?d)" => "($ ?b ?a ?c ?d)"),
        rw!("commute-sum-7"; "($ ?a ?b ?c ?d)" => "($ ?b ?a ?d ?c)"),
        rw!("commute-sum-8"; "($ ?a ?b ?c ?d)" => "($ ?b ?c ?a ?d)"),
        rw!("commute-sum-9"; "($ ?a ?b ?c ?d)" => "($ ?b ?c ?d ?a)"),
        rw!("commute-sum-10"; "($ ?a ?b ?c ?d)" => "($ ?b ?d ?a ?c)"),
        rw!("commute-sum-11"; "($ ?a ?b ?c ?d)" => "($ ?b ?d ?c ?a)"),

        rw!("commute-sum-12"; "($ ?a ?b ?c ?d)" => "($ ?c ?a ?b ?d)"),
        rw!("commute-sum-13"; "($ ?a ?b ?c ?d)" => "($ ?c ?a ?d ?b)"),
        rw!("commute-sum-14"; "($ ?a ?b ?c ?d)" => "($ ?c ?b ?a ?d)"),
        rw!("commute-sum-15"; "($ ?a ?b ?c ?d)" => "($ ?c ?b ?d ?a)"),
        rw!("commute-sum-16"; "($ ?a ?b ?c ?d)" => "($ ?c ?d ?a ?b)"),
        rw!("commute-sum-17"; "($ ?a ?b ?c ?d)" => "($ ?c ?d ?b ?a)"),

        rw!("commute-sum-18"; "($ ?a ?b ?c ?d)" => "($ ?d ?a ?b ?c)"),
        rw!("commute-sum-19"; "($ ?a ?b ?c ?d)" => "($ ?d ?a ?c ?b)"),
        rw!("commute-sum-20"; "($ ?a ?b ?c ?d)" => "($ ?d ?b ?a ?c)"),
        rw!("commute-sum-21"; "($ ?a ?b ?c ?d)" => "($ ?d ?b ?c ?a)"),
        rw!("commute-sum-22"; "($ ?a ?b ?c ?d)" => "($ ?d ?c ?b ?a)"),
        rw!("commute-sum-23"; "($ ?a ?b ?c ?d)" => "($ ?d ?c ?a ?b)"),

        // Neighbors
        rw!("push-in"; "($ ?a ($ ?b ?v ?w ?x) ?y ?z)" => "($ (- ?b) ($ (- ?a) ?v ?w ?x) ?y ?z)"),
        rw!("swap"; "($ ($ ?a ?s ?t ?u) ($ ?b ?v ?w ?x) ?y ?z)" => "($ ($ ?b ?s ?t ?u) ($ ?a ?v ?w ?x) ?y ?z)"),

    ]
}

pub fn rules() -> Vec<Rewrite> { vec![
    rw!("eliminate-add"; "(+ ?a ?b)" => "(- ($ ?a ?b))"),
    rw!("sum-invert"; "($ ?a 0)" => "(- ?a)"),

    // Negation
    rw!("factor-neg"; "($ (- ?a) (- ?b))" => "(- ($ ?a ?b))"),
    rw!("distrib-neg"; "(- ($ ?a ?b))" => "($ (- ?a) (- ?b))"),
    rw!("double-neg"; "(- (- ?a))" => "?a"),
    rw!("neg-double"; "?a" => "(- (- ?a))"),

    rw!("cancel-add"; "($ (- ?a) ?a)" => "0"),

    rw!("add-0"; "($ 0 ?a)" => "(- ?a)"),
    // Commutative properties
    rw!("commute-sum"; "($ ?a ?b)" => "($ ?b ?a)"),

    // Nested commutative
    rw!("push-1"; "($ ?a ($ ?b ?c))" => "($ (- ?b) ($ (- ?a) (?c)))"),
    rw!("swap-2"; "($ ($ ?a ?b) ($ ?c ?d))" => "($ ($ ?c ?b) ($ ?a ?d))"),
]}


pub fn rules4() -> Vec<Rewrite4> { vec![
    rw!("eliminate-add"; "(+ ?a ?b)" => "(- ($2 ?a ?b))"),
    // Negation
    rw!("double-neg"; "(- (- ?a))" => "?a"),
    rw!("neg-double"; "?a" => "(- (- ?a))"),

    rw!("factor-neg2"; "($2 (- ?a) (- ?b))" => "(- ($2 ?a ?b))"),
    rw!("factor-neg3"; "($3 (- ?a) (- ?b) (- ?c))" => "(- ($3 ?a ?b ?c))"),
    rw!("factor-neg4"; "($4 (- ?a) (- ?b) (- ?c) (- ?d))" => "(- ($4 ?a ?b ?c ?d))"),

    rw!("distrib-neg2"; "(- ($2 ?a ?b))" => "($2 (- ?a) (- ?b))"),
    rw!("distrib-neg3"; "(- ($3 ?a ?b ?c))" => "($3 (- ?a) (- ?b) (- ?c))"),
    rw!("distrib-neg4"; "(- ($4 ?a ?b ?c ?d))" => "($4 (- ?a) (- ?b) (- ?c) (- ?d))"),

    // Cancellation
    rw!("cancel-add"; "($2 (- ?a) ?a)" => "0"),
    rw!("downsize-2"; "($2 ?a 0)" => "(- ?a)"),
    rw!("downsize-3"; "($3 ?a ?b 0)" => "($2 ?a ?b)"),
    rw!("downsize-4"; "($4 ?a ?b ?c 0)" => "($3 ?a ?b ?c)"),

    // Commutative properties 2
    rw!("commute-sum2-1"; "($2 ?a ?b)" => "($2 ?b ?a)"),

    // Commutative properties 3
    rw!("commute-sum3-1"; "($3 ?a ?b ?c)" => "($3 ?a ?c ?b)"),
    rw!("commute-sum3-2"; "($3 ?a ?b ?c)" => "($3 ?b ?a ?c)"),
    rw!("commute-sum3-3"; "($3 ?a ?b ?c)" => "($3 ?b ?c ?a)"),
    rw!("commute-sum3-4"; "($3 ?a ?b ?c)" => "($3 ?c ?a ?b)"),
    rw!("commute-sum3-5"; "($3 ?a ?b ?c)" => "($3 ?c ?b ?a)"),

    // Commutative properties 4
    rw!("commute-sum4-1"; "($4 ?a ?b ?c ?d)" => "($4 ?a ?b ?d ?c)"),
    rw!("commute-sum4-2"; "($4 ?a ?b ?c ?d)" => "($4 ?a ?c ?b ?d)"),
    rw!("commute-sum4-3"; "($4 ?a ?b ?c ?d)" => "($4 ?a ?c ?d ?b)"),
    rw!("commute-sum4-4"; "($4 ?a ?b ?c ?d)" => "($4 ?a ?d ?b ?c)"),
    rw!("commute-sum4-5"; "($4 ?a ?b ?c ?d)" => "($4 ?a ?d ?c ?b)"),

    rw!("commute-sum4-6"; "($4 ?a ?b ?c ?d)" => "($4 ?b ?a ?c ?d)"),
    rw!("commute-sum4-7"; "($4 ?a ?b ?c ?d)" => "($4 ?b ?a ?d ?c)"),
    rw!("commute-sum4-8"; "($4 ?a ?b ?c ?d)" => "($4 ?b ?c ?a ?d)"),
    rw!("commute-sum4-9"; "($4 ?a ?b ?c ?d)" => "($4 ?b ?c ?d ?a)"),
    rw!("commute-sum4-10"; "($4 ?a ?b ?c ?d)" => "($4 ?b ?d ?a ?c)"),
    rw!("commute-sum4-11"; "($4 ?a ?b ?c ?d)" => "($4 ?b ?d ?c ?a)"),

    rw!("commute-sum4-12"; "($4 ?a ?b ?c ?d)" => "($4 ?c ?a ?b ?d)"),
    rw!("commute-sum4-13"; "($4 ?a ?b ?c ?d)" => "($4 ?c ?a ?d ?b)"),
    rw!("commute-sum4-14"; "($4 ?a ?b ?c ?d)" => "($4 ?c ?b ?a ?d)"),
    rw!("commute-sum4-15"; "($4 ?a ?b ?c ?d)" => "($4 ?c ?b ?d ?a)"),
    rw!("commute-sum4-16"; "($4 ?a ?b ?c ?d)" => "($4 ?c ?d ?a ?b)"),
    rw!("commute-sum4-17"; "($4 ?a ?b ?c ?d)" => "($4 ?c ?d ?b ?a)"),

    rw!("commute-sum4-18"; "($4 ?a ?b ?c ?d)" => "($4 ?d ?a ?b ?c)"),
    rw!("commute-sum4-19"; "($4 ?a ?b ?c ?d)" => "($4 ?d ?a ?c ?b)"),
    rw!("commute-sum4-20"; "($4 ?a ?b ?c ?d)" => "($4 ?d ?b ?a ?c)"),
    rw!("commute-sum4-21"; "($4 ?a ?b ?c ?d)" => "($4 ?d ?b ?c ?a)"),
    rw!("commute-sum4-22"; "($4 ?a ?b ?c ?d)" => "($4 ?d ?c ?b ?a)"),
    rw!("commute-sum4-23"; "($4 ?a ?b ?c ?d)" => "($4 ?d ?c ?a ?b)"),

    // Nested commutative

    // expand size
    rw!("expand2-2->3"; "($2 ?a ($2 ?b ?c))" => "($3 ?a (- ?b) (- ?c))"),
    rw!("collapse3->2-2"; "($3 ?a ?b ?c)" => "($2 ?a ($2 (- ?b) (- ?c)))"),

    rw!("expand2-3->4"; "($2 ?a ($3 ?b ?c ?d))" => "($4 ?a (- ?b) (- ?c) (- ?d))"),
    rw!("collapse4->2-3"; "($4 ?a ?b ?c ?d)" => "($2 ?a ($3 (- ?b) (- ?c) (- ?d)))"),

    rw!("expand3-2->4"; "($3 ?a ?b ($2 ?c ?d))" => "($4 ?a ?b (- ?c) (- ?d))"),
    rw!("collapse4->3-2"; "($4 ?a ?b ?c ?d)" => "($3 ?a ?b ($2 (- ?c) (- ?d)))"),

    rw!("expand2-2-2->4"; "($2 ($2 (- ?a) (- ?b)) ($2 (- ?c) (- ?d)))" => "($4 ?a ?b ?c ?d)"),
    rw!("collapse4->2-2-2"; "($4 ?a ?b ?c ?d)" => "($2 ($2 (- ?a) (- ?b)) ($2 (- ?c) (- ?d)))"),

    // Nested commutative
    rw!("push-2-2"; "($2 ?a ($2 ?b ?c))" => "($2 (- ?b) ($2 (- ?a) ?c))"),
    rw!("push-2-3"; "($2 ?a ($3 ?b ?c ?d))" => "($2 (- ?b) ($3 (- ?a) ?c ?d))"),
    rw!("push-2-4"; "($2 ?a ($4 ?b ?c ?d ?e))" => "($2 (- ?b) ($4 (- ?a) ?c ?d ?e))"),

    rw!("push-3-2"; "($3 ?a ($2 ?b ?c) ?z)" => "($3 (- ?b) ($2 (- ?a) ?c) ?z)"),
    rw!("push-3-3"; "($3 ?a ($3 ?b ?c ?d) ?z)" => "($3 (- ?b) ($3 (- ?a) ?c ?d) ?z)"),
    rw!("push-3-4"; "($3 ?a ($4 ?b ?c ?d ?e) ?z)" => "($3 (- ?b) ($4 (- ?a) ?c ?d ?e) ?z)"),

    rw!("push-4-2"; "($4 ?a ($2 ?b ?c) ?y ?z)" => "($4 (- ?b) ($2 (- ?a) ?c) ?y ?z)"),
    rw!("push-4-3"; "($4 ?a ($3 ?b ?c ?d) ?y ?z)" => "($4 (- ?b) ($3 (- ?a) ?c ?d) ?y ?z)"),
    rw!("push-4-4"; "($4 ?a ($4 ?b ?c ?d ?e) ?y ?z)" => "($4 (- ?b) ($4 (- ?a) ?c ?d ?e) ?y ?z)"),

    rw!("swap-2-2-2"; "($2 ($2 ?a ?b) ($2 ?c ?d))" => "($2 ($2 ?c ?b) ($2 ?a ?d))"),
    rw!("swap-2-2-3"; "($2 ($2 ?a ?b) ($3 ?c ?d ?e))" => "($2 ($2 ?c ?b) ($3 ?a ?d ?e))"),
    rw!("swap-2-2-4"; "($2 ($2 ?a ?b) ($4 ?c ?d ?e ?f))" => "($2 ($2 ?c ?b) ($4 ?a ?d ?e ?f))"),
    rw!("swap-2-3-3"; "($2 ($3 ?a ?b ?c) ($3 ?d ?e ?f))" => "($2 ($3 ?d ?b ?c) ($3 ?a ?e ?f))"),
    rw!("swap-2-3-4"; "($2 ($3 ?a ?b ?c) ($4 ?d ?e ?f ?g))" => "($2 ($3 ?d ?b ?c) ($4 ?a ?e ?f ?g))"),
    rw!("swap-2-4-4"; "($2 ($4 ?a ?b ?c ?d) ($4 ?e ?f ?g ?h))" => "($2 ($4 ?e ?b ?c ?d) ($4 ?a ?f ?g ?h))"),

    rw!("swap-3-2-2"; "($3 ($2 ?a ?b) ($2 ?c ?d) ?y)" => "($3 ($2 ?c ?b) ($2 ?a ?d) ?y)"),
    rw!("swap-3-2-3"; "($3 ($2 ?a ?b) ($3 ?c ?d ?e) ?y)" => "($3 ($2 ?c ?b) ($3 ?a ?d ?e) ?y)"),
    rw!("swap-3-2-4"; "($3 ($2 ?a ?b) ($4 ?c ?d ?e ?f) ?y)" => "($3 ($2 ?c ?b) ($4 ?a ?d ?e ?f) ?y)"),
    rw!("swap-3-3-3"; "($3 ($3 ?a ?b ?c) ($3 ?d ?e ?f) ?y)" => "($3 ($3 ?d ?b ?c) ($3 ?a ?e ?f) ?y)"),
    rw!("swap-3-3-4"; "($3 ($3 ?a ?b ?c) ($4 ?d ?e ?f ?g) ?y)" => "($3 ($3 ?d ?b ?c) ($4 ?a ?e ?f ?g) ?y)"),
    rw!("swap-3-4-4"; "($3 ($4 ?a ?b ?c ?d) ($4 ?e ?f ?g ?h) ?y)" => "($3 ($4 ?e ?b ?c ?d) ($4 ?a ?f ?g ?h) ?y)"),


    rw!("swap-4-2-2"; "($4 ($2 ?a ?b) ($2 ?c ?d) ?y ?z)" => "($4 ($2 ?c ?b) ($2 ?a ?d) ?y ?z)"),
    rw!("swap-4-2-3"; "($4 ($2 ?a ?b) ($3 ?c ?d ?e) ?y ?z)" => "($4 ($2 ?c ?b) ($3 ?a ?d ?e) ?y ?z)"),
    rw!("swap-4-2-4"; "($4 ($2 ?a ?b) ($4 ?c ?d ?e ?f) ?y ?z)" => "($4 ($2 ?c ?b) ($4 ?a ?d ?e ?f) ?y ?z)"),
    rw!("swap-4-3-3"; "($4 ($3 ?a ?b ?c) ($3 ?d ?e ?f) ?y ?z)" => "($4 ($3 ?d ?b ?c) ($3 ?a ?e ?f) ?y ?z)"),
    rw!("swap-4-3-4"; "($4 ($3 ?a ?b ?c) ($4 ?d ?e ?f ?g) ?y ?z)" => "($4 ($3 ?d ?b ?c) ($4 ?a ?e ?f ?g) ?y ?z)"),
    rw!("swap-4-4-4"; "($4 ($4 ?a ?b ?c ?d) ($4 ?e ?f ?g ?h) ?y ?z)" => "($4 ($4 ?e ?b ?c ?d) ($4 ?a ?f ?g ?h) ?y ?z)"),

]}

fn main() {
    let expr: RecExpr<LinDiff0> = "(+ (+ a c) (- (+ (- b) b)))".parse().unwrap();
    // let expr: RecExpr<LinDiff> = "(+ (+ x z) (+ w (- x)))".parse().unwrap();
    // let expr: RecExpr<LinDiff> = "(- (+ x (- x)))".parse().unwrap();
    let r = rules0();
    let runner = Runner::default().with_expr(&expr).run(&r);

    let extractor = Extractor::new(&runner.egraph, LinDiff0CostFn);
    // let extractor = Extractor::new(&runner.egraph, AstSize);
    let (best_cost, best_expr) = extractor.find_best(runner.roots[0]);

    println!("Cost: {}", best_cost);
    println!("Expr: {}", best_expr);
//    runner.egraph.dot().to_pdf("graph.pdf").unwrap();
}

egg::test_fn! {
    cancellation_1, rules(), "($ (- a) a c d)" => "($ c d 0 0)"
}

egg::test_fn! {
    cancellation_2, rules(), "($ (- a) a c 0)" => "($ c 0 0 0)"
}

egg::test_fn! {
    cancellation_3, rules(), "($ (- a) a 0 c)" => "($ c 0 0 0)"
}

egg::test_fn! {
    cancellation_4, rules(), "(+ (+ x z) (+ w (- x)))" => "(- ($ z w 0 0))"
}

egg::test_fn! {
    complicated, rules(), "(+ c (- (+ (- b) (+ a b))))" => "($ (- a) c 0 0)"
}
