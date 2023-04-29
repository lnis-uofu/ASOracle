use egg::{rewrite as rw, *};

define_language! {
    pub enum LinDiff4 {
        "i" = Integrate(Id),
        "$" = Summing([Id; 4]),
        "*" = Weight([Id; 2]),
        Constant(u32),
        Symbol(Symbol),
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
            LinDiff4::Weight(..) => 0,
            LinDiff4::Symbol(..) => 0,
            LinDiff4::Constant(..) => 0,
            _ => 1,
        };
        enode.fold(op_cost, |sum, i| sum + costs(i))
    }
}

define_language! {
    pub enum LinDiff4X {
        // Non-inverting versions
        "+" = Add([Id; 2]),
        "I" = Integrate(Id),
        "x" = Mult([Id; 2]),
        "-" = Negative(Id),
        // Inverting amp versions
        "$" = Summing([Id; 4]),
        "i" = Integrating(Id),
        "*" = Weight([Id; 2]),

        // Constant combination
        "c*" = ConstMult([Id; 2]),
        "c+" = ConstAdd([Id; 2]),
        "c-" = ConstSub([Id; 2]),
        Constant(u32),
        Symbol(Symbol),
    }
}


pub struct LinDiff4XCostFn;
impl egg::CostFunction<LinDiff4X> for LinDiff4XCostFn {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &LinDiff4X, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            LinDiff4X::Add(..) => 10000,
            LinDiff4X::Integrate(..) => 10000,
            LinDiff4X::Mult(..) => 10000,
            LinDiff4X::Symbol(..) => 0,
            LinDiff4X::ConstMult(..) => 0,
            LinDiff4X::ConstAdd(..) => 0,
            LinDiff4X::ConstSub(..) => 0,
            LinDiff4X::Constant(..) => 0,
            LinDiff4X::Weight(..) => 0,
            _ => 1,
        };
        enode.fold(op_cost, |sum, i| sum + costs(i))
    }
}

pub fn rules4_integrate<L: Language + Send + Sync + FromOp + 'static, N: Analysis<L> + 'static>() -> Vec<egg::Rewrite<L, N>> {
    vec![
        rw!("integrate-split"; "(i ($ ?a ?b ?c ?d))" => "($ (i ?a) (i ?b) (i ?c) (i ?d))"),
        rw!("integrate-join"; "($ (i ?a) (i ?b) (i ?c) (i ?d))" => "(i ($ ?a ?b ?c ?d))"),
        rw!("integrate-zero"; "(i 0)" => "0"),
    ]
}

pub fn rules4_core<L: Language + Send + Sync + FromOp + 'static, N: Analysis<L> + 'static>() -> Vec<egg::Rewrite<L, N>> {
    vec![
        // Negation
        rw!("neg-zero"; "($ 0 0 0 0)" => "0"),
        rw!("zero-neg"; "0" => "($ 0 0 0 0)"),

        rw!("double-neg"; "($ ($ ?a 0 0 0) 0 0 0)" => "?a"),
        rw!("neg-double"; "?a" => "($ ($ ?a 0 0 0) 0 0 0)"),

        // Commutative properties
        rw!("commute-sum-ab"; "($ ?a ?b ?c ?d)" => "($ ?b ?a ?c ?d)"),
        rw!("commute-sum-ac"; "($ ?a ?b ?c ?d)" => "($ ?c ?b ?a ?d)"),
        rw!("commute-sum-ad"; "($ ?a ?b ?c ?d)" => "($ ?d ?b ?c ?a)"),
        rw!("commute-sum-bc"; "($ ?a ?b ?c ?d)" => "($ ?a ?c ?b ?d)"),
        rw!("commute-sum-bd"; "($ ?a ?b ?c ?d)" => "($ ?a ?d ?c ?b)"),
        rw!("commute-sum-cd"; "($ ?a ?b ?c ?d)" => "($ ?a ?b ?d ?c)"),

        // Nested commutative
        rw!("swap"; "($ ($ ?a ?s ?t ?u) ($ ?b ?v ?w ?x) ?y ?z)" =>
            "($ ($ ?b ?s ?t ?u) ($ ?a ?v ?w ?x) ?y ?z)"),
    ]
}

pub fn rules4_shortcuts<L: Language + Send + Sync + FromOp + 'static, N: Analysis<L> + 'static>() -> Vec<egg::Rewrite<L, N>> {
    vec![
        // would be implicit from swap and neg-zero
        rw!("factor-neg"; "($ ($ ?a 0 0 0) ($ ?b 0 0 0) ?c ?d)" => "($ ($ ?a ?b 0 0) 0 ?c ?d)"),
        // would be implicit from zero-neg and swap
        rw!("distrib-neg"; "($ ($ ?a ?b 0 0) 0 ?c ?d)" => "($ ($ ?a 0 0 0) ($ ?b 0 0 0) ?c ?d)"),
        // Would be implicit from double-neg swap double-neg
        rw!("cancel-add"; "($ ($ ?a 0 0 0) ?a ?b ?c) " => "($ 0 0 ?b ?c)"),
        // implicit from neg-double and swap
        rw!("push-in"; "($ ?a ($ ?b ?x ?y ?z) ?v ?w)" =>
            "($ ($ ?b 0 0 0) ($ ($ ?a 0 0 0) ?x ?y ?z) ?v ?w)"),
    ]
}

pub fn rules4_weights<L: Language + Send + Sync + FromOp + 'static, N: Analysis<L> + 'static>() -> Vec<egg::Rewrite<L, N>> {
    vec![
        // Weights
        rw!("factor-integrate"; "(i (* ?a ?b))" => "(* ?a (i ?b))"),
        rw!("distribute-integrate"; "(* ?a (i ?b))" => "(i (* ?a ?b))"),

        rw!("factor-weight"; "($ (* ?m ?a) (* ?m ?b) (* ?m ?c) (* ?m ?d))" =>
            "(* ?m ($ ?a ?b ?c ?d))"),
        rw!("distribute-weight"; "(* ?m ($ ?a ?b ?c ?d))" =>
            "($ (* ?m ?a) (* ?m ?b) (* ?m ?c) (* ?m ?d))"),

        rw!("weight-unit"; "(* 1 ?a)" => "?a"),
        rw!("unit-weight"; "?a" => "(* 1 ?a)"),
        rw!("weight-zero"; "(* 0 ?a)" => "0"),
        rw!("weight-commutative"; "(* ?a (* ?b ?c))" => "(* ?b (* ?a ?c))"),
    ]
}

//impl<LinDiff4X> fn is_greater_than(v: &str, w: &str) -> impl Fn(&mut EGraph4X, Id, &Subst) -> bool {
//     let v = v.parse().unwrap();
//     let w = w.parse().unwrap();
//     move |egraph, _, subst| {
//         match (&egraph[subst[v]].nodes[0], &egraph[subst[w]].nodes[0]) {
//             (LinDiff4X::Constant(m), LinDiff4X::Constant(n)) => m > n,
//             _ => false
//         }
//     }
// }

pub fn rules4_weight_combine<L: Language + Send + Sync + FromOp + 'static, N: Analysis<L> + 'static>() -> Vec<egg::Rewrite<L, N>> {
    vec![
        rw!("constant-mult-commute"; "(c* ?a ?b)" => "(c* ?b ?a)"),
        rw!("constant-add-commute"; "(c+ ?a ?b)" => "(c+ ?b ?a)"),

        rw!("weight-merge"; "(* ?a (* ?b ?z))" => "(* (c* ?a ?b) ?z))"),
        rw!("weight-add"; "($ (* ?m ?a) (* ?n ?a) ?b ?c)" => "($ (* (c+ ?m ?n) ?a) ?b ?c 0)"),
        rw!("weight-sub"; "($ (* ?m ?a) (* ?n ($ ?a ?x ?y ?z)) ?b ?c)" =>
            "($ (* (c- ?m ?n) ?a) (* ?n ($ ?x ?y ?z 0)) ?b ?c)"
        // if is_greater_than<T>("?m", "?n")
        ),
    ]
}

pub fn rules4_elimination<L: Language + Send + Sync + FromOp + 'static, N: Analysis<L> + 'static>() -> Vec<egg::Rewrite<L, N>> {
    vec![
        rw!("eliminate-add"; "(+ ?a ?b)" => "($ ($ ?a ?b 0 0) 0 0 0)"),
        rw!("eliminate-integral"; "(I ?a)" => "($ (i ?a) 0 0 0)"),
        rw!("eliminate-neg"; "(- ?a)" => "($ ?a 0 0 0)"),
        rw!("eliminate-mult"; "(x ?a ?b)" => "(* ?a ?b)"),
    ]
}

pub fn rules4() -> Vec<egg::Rewrite<LinDiff4X, ()>> {
    let mut rules = rules4_core();
    rules.extend(rules4_shortcuts());
    rules.extend(rules4_elimination());
    rules.extend(rules4_integrate());
    rules.extend(rules4_weights());
    rules.extend(rules4_weight_combine());
    rules
}

egg::test_fn!{
    commutative, rules4(), "($ a b c d)" => "($ d c b a)"
}

egg::test_fn! {
    unit_weights, rules4(), "(+ x y)" => "($ ($ (* 1 x) (* 1 y) 0 0) 0 0 0)"
}

egg::test_fn! {
    unit_weights_neg, rules4(), "(- (+ x y))" => "($ (* 1 x) (* 1 y) 0 0)"
}
egg::test_fn! {
    cancellation_1, rules4(), "($ (- a) a c d)" => "($ c d 0 0)"
}

egg::test_fn! {
    cancellation_2, rules4(), "($ (- a) a c 0)" => "($ c 0 0 0)"
}

egg::test_fn! {
    cancellation_3, rules4(), "($ (- a) a 0 c)" => "($ c 0 0 0)"
}

egg::test_fn! {
    cancellation_4, rules4(), "(+ (+ x z) (+ w (- x)))" => "($ ($ z w 0 0) 0 0 0)"
}

egg::test_fn! {
    cancellation_complicated, rules4(), "(+ c (- (+ (- b) (+ a b))))" =>
        "($ ($ ($ a 0 0 0) c 0 0) 0 0 0)"
}

egg::test_fn! {
    consolidate, rules4(), "(+ a (+ b (+ c d)))" =>
        "($ ($ a b c d) 0 0 0)"
}

// Isn't capable of proving this
// egg::test_fn! {
//     balance, rules4(), "(+ a (+ b (+ c (+ d e))))" =>
//         "($ ($ a 0 0 0) ($ b c d e) 0 0)"
// }
