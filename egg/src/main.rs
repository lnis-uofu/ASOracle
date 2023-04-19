use egg::{rewrite as rw, *};


// four child summing version
pub type EGraph = egg::EGraph<LinDiff, ()>;
pub type Rewrite = egg::Rewrite<LinDiff, ()>;

define_language! {
    pub enum LinDiff {
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

pub struct LinDiffCostFn;
impl egg::CostFunction<LinDiff> for LinDiffCostFn {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &LinDiff, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            LinDiff::Add(..) => 2,
            LinDiff::Integrate(..) => 2,
            LinDiff::Symbol(..) => 0,
            _ => 1,
        };
        enode.fold(op_cost, |sum, i| sum + costs(i))
    }
}

pub fn rules() -> Vec<Rewrite> {
    vec![
        rw!("eliminate-add"; "(+ ?a ?b)" => "(- ($ ?a ?b 0 0))"),
        rw!("eliminate-integral"; "(i ?a)" => "(- (I ?a))"),

        rw!("integrate-split"; "(I ($ ?a ?b ?c ?d))" => "($ (I ?a) (I ?b) (I ?c) (I ?d))"),
        rw!("integrate-join"; "($ (I ?a) (I ?b) (I ?c) (I ?d))" => "(I ($ ?a ?b ?c ?d))"),

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
        rw!("push-in"; "($ ?a ($ ?b ?v ?w ?x) ?y ?z)" =>
            "($ (- ?b) ($ (- ?a) ?v ?w ?x) ?y ?z)"),
        rw!("swap"; "($ ($ ?a ?s ?t ?u) ($ ?b ?v ?w ?x) ?y ?z)" =>
            "($ ($ ?b ?s ?t ?u) ($ ?a ?v ?w ?x) ?y ?z)"),

    ]
}

pub type EGraph4w = egg::EGraph<LinDiff4w, ()>;
pub type Rewrite4w = egg::Rewrite<LinDiff4w, ()>;

define_language! {
    pub enum LinDiff4w {
        // Non-inverting versions
        "+" = Add([Id; 2]),
        "i" = Integrate(Id),
        "*" = Mult([Id; 2]),

        // Inverting amp versions
        "$" = Summing([Id; 4]),
        "I" = Integrating(Id),
        "w" = Weight([Id; 2]),
        // Negative is effectively summing with one input
        "-" = Negative(Id),
        // Constant combination
        "c*" = ConstMult([Id; 2]),
        "c+" = ConstAdd([Id; 2]),
        "c-" = ConstSub([Id; 2]),
        Constant(i32),
        Symbol(Symbol),
    }
}

fn is_greater_than_4w(v: &str, w: &str) -> impl Fn(&mut EGraph4w, Id, &Subst) -> bool {
    let v = v.parse().unwrap();
    let w = w.parse().unwrap();
    move |egraph, _, subst| {
        match (&egraph[subst[v]].nodes[0], &egraph[subst[w]].nodes[0]) {
            (LinDiff4w::Constant(m), LinDiff4w::Constant(n)) => m > n,
            _ => false
        }
    }
}


pub struct LinDiff4wCostFn;
impl egg::CostFunction<LinDiff4w> for LinDiff4wCostFn {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &LinDiff4w, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            // These should be eliminated.
            LinDiff4w::Add(..) => 10000,
            LinDiff4w::Integrate(..) => 10000,
            LinDiff4w::Mult(..) => 10000,
            // These have no overhead.
            LinDiff4w::Symbol(..) => 0,
            LinDiff4w::Constant(..) => 0,
            LinDiff4w::ConstMult(..) => 0,
            LinDiff4w::ConstAdd(..) => 0,
            LinDiff4w::ConstSub(..) => 0,
            LinDiff4w::Weight(..) => 0,
            // Everything else is a size 1 amp.
            _ => 1
        };
        enode.fold(op_cost, |sum, i| sum + costs(i))
    }
}

pub fn rules4w() -> Vec<Rewrite4w> {
    vec![
        rw!("eliminate-add"; "(+ ?a ?b)" => "(- ($ ?a ?b 0 0))"),
        rw!("eliminate-integral"; "(i ?a)" => "(- (I ?a))"),
        rw!("eliminate-mult"; "(* ?a ?b)" => "(- ($ (w ?a ?b) 0 0 0))"),

        rw!("integrate-split"; "(I ($ ?a ?b ?c ?d))" => "($ (I ?a) (I ?b) (I ?c) (I ?d))"),
        rw!("integrate-join"; "($ (I ?a) (I ?b) (I ?c) (I ?d))" => "(I ($ ?a ?b ?c ?d))"),

        rw!("invert-sum"; "(- ?a)" => "($ ?a 0 0 0)"),
        rw!("sum-invert"; "($ ?a 0 0 0)" => "(- ?a)"),

        // Negation
        rw!("factor-neg"; "($ (- ?a) (- ?b) (- ?c) (- ?d))" => "(- ($ ?a ?b ?c ?d))"),
        rw!("distrib-neg"; "(- ($ ?a ?b ?c ?d))" => "($ (- ?a) (- ?b) (- ?c) (- ?d))"),
        rw!("integrate-neg-factor"; "(I (- ?a))" => "(- (I ?a))"),
        rw!("integrate-neg-dist"; "(- (I ?a))" => "(I (- ?a))"),

        rw!("integrate-0"; "(I 0)" => "0"),
        rw!("0-neg"; "0" => "(- 0)"),
        rw!("neg-0"; "(- 0)" => "0"),
        rw!("double-neg"; "(- (- ?a))" => "?a"),
        rw!("neg-double"; "?a" => "(- (- ?a))"),

        rw!("cancel-add"; "($ (- ?a) ?a ?b ?c)" => "($ ?b ?c 0 0)"),

        rw!("factor-weight"; "($ (w ?m ?a) (w ?m ?b) (w ?m ?c) (w ?m ?d))" =>
            "(w ?m ($ (w 1 ?a) (w 1 ?b) (w 1 ?c) (w 1 ?d)))"),
        rw!("distribute-weight-unit"; "(w ?m ($ (w 1 ?a) (w 1 ?b) (w 1 ?c) (w 1 ?d)))" =>
            "($ (w ?m ?a) (w ?m ?b) (w ?m ?c) (w ?m ?d))"),
        rw!("distribute-weight"; "(w ?m ($ (w ?q ?a) (w ?r ?b) (w ?s ?c) (w ?t ?d)))" =>
            "($ (w (c* ?m ?q) ?a) (w (c* ?m ?r) ?b) (w (c* ?m ?s) ?c) (w (c* ?m ?t) ?d))"),

        rw!("weight-1"; "(w 1 (w ?a ?b))" => "(w ?a ?b)"),
        rw!("weight-0"; "(w 0 ?a)" => "0"),
        // Really should be able to collapse these with an analyzer
        rw!("weight-merge"; "(w ?a (w ?b ?z))" => "(w (c* ?a ?b) ?z))"),
        rw!("weight-neg"; "(w ?a (- ?b))" => "(- (w ?a ?b))"),
        rw!("weight-distrib"; "(- (w ?a ?b))" => "(w ?a (- ?b))"),
        rw!("weight-add"; "($ (w ?m ?a) (w ?n ?a) ?b ?c)" => "($ (w (c+ ?m ?n) ?a) ?b ?c 0)"),
        rw!("weight-sub"; "($ (w ?m ?a) (- (w ?n ?a)) ?b ?c)" => "($ (w (c- ?m ?n) ?a) ?b ?c 0)"
            if is_greater_than_4w("?m", "?n")),

        // Neighbors
        rw!("push-in"; "($ ?a ($ ?b ?v ?w ?x) ?y ?z)" =>
            "($ (- ?b) ($ (- ?a) ?v ?w ?x) ?y ?z)"),
        rw!("swap"; "($ ($ ?a ?s ?t ?u) ($ ?b ?v ?w ?x) ?y ?z)" =>
            "($ ($ ?b ?s ?t ?u) ($ ?a ?v ?w ?x) ?y ?z)"),

        // Commutative properties
        rw!("commute-sum-1"; "($ ?a ?b ?c ?d)" => "($ ?b ?a ?c ?d)"),
        rw!("commute-sum-2"; "($ ?a ?b ?c ?d)" => "($ ?c ?b ?a ?d)"),
        rw!("commute-sum-3"; "($ ?a ?b ?c ?d)" => "($ ?d ?b ?c ?a)"),
        rw!("commute-sum-4"; "($ ?a ?b ?c ?d)" => "($ ?a ?c ?b ?d)"),
        rw!("commute-sum-5"; "($ ?a ?b ?c ?d)" => "($ ?a ?d ?c ?b)"),
        rw!("commute-sum-6"; "($ ?a ?b ?c ?d)" => "($ ?a ?b ?d ?c)"),
    ]
}


// two child summing version
pub type EGraph2 = egg::EGraph<LinDiff2, ()>;
pub type Rewrite2 = egg::Rewrite<LinDiff2, ()>;
define_language! {
    pub enum LinDiff2 {
        "+" = Add([Id; 2]),
        "i" = Integrate(Id),
        "$" = Summing([Id; 2]),
        "I" = Integrating(Id),
        "-" = Negative(Id),
        Constant(i32),
        Symbol(Symbol),
    }
}

pub struct LinDiff2CostFn;
impl egg::CostFunction<LinDiff2> for LinDiff2CostFn {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &LinDiff2, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            LinDiff2::Add(..) => 2,
            LinDiff2::Integrate(..) => 2,
            LinDiff2::Symbol(..) => 0,
            _ => 1,
        };
        enode.fold(op_cost, |sum, i| sum + costs(i))
    }
}

pub fn rules2() -> Vec<Rewrite2> {
    vec![
        rw!("eliminate-add"; "(+ ?a ?b)" => "(- ($ ?a ?b))"),
        rw!("eliminate-integral"; "(i ?a)" => "(- (I ?a))"),
        rw!("sum-invert"; "($ ?a 0)" => "(- ?a)"),

        rw!("integrate-split"; "(I ($ ?a ?b))" => "($ (I ?a) (I ?b))"),
        rw!("integrate-join"; "($ (I ?a) (I ?b))" => "(I ($ ?a ?b))"),

        // Negation
        rw!("factor-neg"; "($ (- ?a) (- ?b))" => "(- ($ ?a ?b))"),
        rw!("distrib-neg"; "(- ($ ?a ?b))" => "($ (- ?a) (- ?b))"),
        rw!("double-neg"; "(- (- ?a))" => "?a"),
        rw!("neg-double"; "?a" => "(- (- ?a))"),
        rw!("integrate-0"; "(I 0)" => "0"),

        rw!("cancel-add"; "($ (- ?a) ?a)" => "0"),

        rw!("add-0"; "($ 0 ?a)" => "(- ?a)"),
        // Commutative properties
        rw!("commute-sum"; "($ ?a ?b)" => "($ ?b ?a)"),

        // Nested commutative
        rw!("push-1"; "($ ?a ($ ?b ?c))" => "($ (- ?b) ($ (- ?a) (?c)))"),
        rw!("swap-2"; "($ ($ ?a ?b) ($ ?c ?d))" => "($ ($ ?c ?b) ($ ?a ?d))"),
    ]
}

// two child summing version with weights
pub type EGraph2w = egg::EGraph<LinDiff2w, ()>;
pub type Rewrite2w = egg::Rewrite<LinDiff2w, ()>;
define_language! {
    pub enum LinDiff2w {
        "+" = Add([Id; 2]),
        "*" = Mult([Id; 2]),
        "i" = Integrate(Id),
        "$" = Summing([Id; 2]),
        "I" = Integrating(Id),
        "w" = Weight([Id; 2]),
        "-" = Negative(Id),
        "c*" = ConstMult([Id; 2]),
        "c+" = ConstAdd([Id; 2]),
        "c-" = ConstSub([Id; 2]),
        Constant(u32),
        Symbol(egg::Symbol),
    }
}

pub struct LinDiff2wCostFn;
impl egg::CostFunction<LinDiff2w> for LinDiff2wCostFn {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &LinDiff2w, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            LinDiff2w::Add(..) => 10000,
            LinDiff2w::Integrate(..) => 10000,
            LinDiff2w::Mult(..) => 10000,
            LinDiff2w::Symbol(..) => 0,
            LinDiff2w::ConstMult(..) => 0,
            LinDiff2w::ConstAdd(..) => 0,
            LinDiff2w::ConstSub(..) => 0,
            LinDiff2w::Constant(..) => 0,
            LinDiff2w::Weight(..) => 0,
            _ => 1,
        };
        enode.fold(op_cost, |sum, i| sum + costs(i))
    }
}

fn is_greater_than(v: &str, w: &str) -> impl Fn(&mut EGraph2w, Id, &Subst) -> bool {
    let v = v.parse().unwrap();
    let w = w.parse().unwrap();
    move |egraph, _, subst| {
        match (&egraph[subst[v]].nodes[0], &egraph[subst[w]].nodes[0]) {
            (LinDiff2w::Constant(m), LinDiff2w::Constant(n)) => m > n,
            _ => false
        }
    }
}


pub fn rules2w() -> Vec<Rewrite2w> {
    vec![
        rw!("eliminate-add"; "(+ ?a ?b)" => "(- ($ (w 1 ?a) (w 1 ?b)))"),
        rw!("eliminate-integral"; "(i ?a)" => "(- (I ?a))"),
        rw!("eliminate-mult"; "(* ?a ?b)" => "(- ($ (w ?a ?b) 0))"),
        rw!("eliminate-neg"; "(- ?a)"  => "($ ?a 0)"),
        rw!("add-0"; "($ ?a 0)" => "(- ?a)"),

        rw!("integrate-split"; "(I ($ ?a ?b))" => "($ (I ?a) (I ?b))"),
        rw!("integrate-join"; "($ (I ?a) (I ?b))" => "(I ($ ?a ?b))"),

        // Negation
        rw!("factor-neg"; "($ (- ?a) (- ?b))" => "(- ($ ?a ?b))"),
        rw!("distrib-neg"; "(- ($ ?a ?b))" => "($ (- ?a) (- ?b))"),
        rw!("integrate-neg-factor"; "(I (- ?a))" => "(- (I ?a))"),
        rw!("integrate-neg-distrib"; "(- (I ?a))" => "(I (- ?a))"),

        rw!("integrate-0"; "(I 0)" => "0"),
        rw!("double-neg"; "(- (- ?a))" => "?a"),
        rw!("neg-double"; "?a" => "(- (- ?a))"),

        rw!("cancel-add"; "($ (- ?a) ?a)" => "0"),

        // Weights
        rw!("factor-weight"; "($ (w ?m ?a) (w ?m ?b))" => "(w ?m ($ (w 1 ?a) (w 1 ?b)))"),
        rw!("distribute-weight-unit"; "(w ?m ($ (w 1 ?a) (w 1 ?b)))" =>
            "($ (w ?m ?a) (w ?m ?b))"),
        rw!("distribute-weight"; "(w ?m ($ (w ?n ?a) (w ?p ?b)))" =>
            "($ (w (c* ?m ?n) ?a) (w (c* ?m ?p) ?b))"),

        rw!("weight-1"; "(w 1 (w ?a ?b))" => "(w ?a ?b)"),
        rw!("weight-0"; "(w 0 ?a)" => "0"),
        // Really should be able to collapse these with an analyzer
        rw!("weight-merge"; "(w ?a (w ?b ?z))" => "(w (c* ?a ?b) ?z))"),
        rw!("weight-neg"; "(w ?a (- ?b))" => "(- (w ?a ?b))"),
        rw!("weight-distrib"; "(- (w ?a ?b))" => "(w ?a (- ?b))"),
        rw!("weight-add"; "($ (w ?m ?a) (w ?n ?a))" => "(w (c+ ?m ?n) ?a)"),
        rw!("weight-sub"; "($ (w ?m ?a) (w ?n (- ?a)))" => "(w (c- ?m ?n) ?a)"
            if is_greater_than("?m", "?n")),
        // Commutative properties
        rw!("commute-sum"; "($ ?a ?b)" => "($ ?b ?a)"),

        // Nested commutative
        rw!("push"; "($ ?a ($ ?b ?c))" => "($ (- ?b) ($ (- ?a) (?c)))"),
        rw!("swap"; "($ ($ ?a ?b) ($ ?c ?d))" => "($ ($ ?c ?b) ($ ?a ?d))"),
    ]
}

fn main() {
    let expr: RecExpr<LinDiff4w> = "(+ (- (+ (- (* 6 j)) (* 1 j))) (+ (* 4 h) (* 5 g)))".parse().unwrap();
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
