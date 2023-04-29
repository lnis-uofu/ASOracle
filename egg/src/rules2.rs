use egg::{rewrite as rw, *};

define_language! {
    pub enum LinDiff2 {
        "i" = Integrate(Id),
        "$" = Summing([Id; 2]),
        "*" = Weight([Id; 2]),
        Constant(u32),
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
            LinDiff2::Weight(..) => 0,
            LinDiff2::Symbol(..) => 0,
            LinDiff2::Constant(..) => 0,
            _ => 1,
        };
        enode.fold(op_cost, |sum, i| sum + costs(i))
    }
}

define_language! {
    pub enum LinDiff2X {
        "+" = Add([Id; 2]),
        "x" = Mult([Id; 2]),
        "I" = Integrate(Id),
        "$" = Summing([Id; 2]),
        "i" = Integrating(Id),
        "*" = Weight([Id; 2]),
        "-" = Negative(Id),
        "c*" = ConstMult([Id; 2]),
        "c+" = ConstAdd([Id; 2]),
        "c-" = ConstSub([Id; 2]),
        Constant(u32),
        Symbol(egg::Symbol),
    }
}

pub struct LinDiff2XCostFn;
impl egg::CostFunction<LinDiff2X> for LinDiff2XCostFn {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &LinDiff2X, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            LinDiff2X::Add(..) => 10000,
            LinDiff2X::Integrate(..) => 10000,
            LinDiff2X::Mult(..) => 10000,
            LinDiff2X::Symbol(..) => 0,
            LinDiff2X::ConstMult(..) => 0,
            LinDiff2X::ConstAdd(..) => 0,
            LinDiff2X::ConstSub(..) => 0,
            LinDiff2X::Constant(..) => 0,
            LinDiff2X::Weight(..) => 0,
            _ => 1,
        };
        enode.fold(op_cost, |sum, i| sum + costs(i))
    }
}

pub fn rules2_integrate<L: Language + Send + Sync + FromOp + 'static, N: Analysis<L> + 'static>() -> Vec<egg::Rewrite<L, N>> {
    vec![
        rw!("integrate-split"; "(i ($ ?a ?b))" => "($ (i ?a) (i ?b))"),
        rw!("integrate-join"; "($ (i ?a) (i ?b))" => "(i ($ ?a ?b))"),
        rw!("integrate-zero"; "(i 0)" => "0"),
    ]
}

pub fn rules2_core<L: Language + Send + Sync + FromOp + 'static, N: Analysis<L> + 'static>() -> Vec<egg::Rewrite<L, N>> {
    vec![
        // Negation
        rw!("neg-zero"; "($ 0 0)" => "0"),
        rw!("zero-neg"; "0" => "($ 0 0)"),

        rw!("double-neg"; "($ ($ ?a 0) 0)" => "?a"),
        rw!("neg-double"; "?a" => "($ ($ ?a 0) 0)"),
        // Commutative properties
        rw!("commute-sum"; "($ ?a ?b)" => "($ ?b ?a)"),
        // Nested commutative
        rw!("swap"; "($ ($ ?a ?b) ($ ?c ?d))" => "($ ($ ?c ?b) ($ ?a ?d))"),
    ]
}

pub fn rules2_shortcuts<L: Language + Send + Sync + FromOp + 'static, N: Analysis<L> + 'static>() -> Vec<egg::Rewrite<L, N>> {
    vec![
        // would be implicit from swap and neg-zero
        rw!("factor-neg"; "($ ($ ?a 0) ($ ?b 0))" => "($ ($ ?a ?b) 0)"),
        // would be implicit from zero-neg and swap
        rw!("distrib-neg"; "($ ($ ?a ?b) 0)" => "($ ($ ?a 0) ($ ?b 0))"),
        // Would be implicit from double-neg swap double-neg
        rw!("cancel-add"; "($ ($ ?a ?c) ?a)" => "?c"),
        // implicit from neg-double and swap
        rw!("push-in"; "($ ?a ($ ?b ?c))" => "($ ($ ?b 0) ($ ($ ?a 0) ?c))"),
    ]
}

pub fn rules2_weights<L: Language + Send + Sync + FromOp + 'static, N: Analysis<L> + 'static>() -> Vec<egg::Rewrite<L, N>> {
    vec![
        // Weights
        rw!("factor-integrate"; "(i (* ?a ?b))" => "(* ?a (i ?b))"),
        rw!("distribute-integrate"; "(* ?a (i ?b))" => "(i (* ?a ?b))"),

        rw!("factor-weight"; "($ (* ?m ?a) (* ?m ?b))" => "(* ?m ($ ?a ?b))"),
        rw!("distribute-weight"; "(* ?m ($ ?a ?b))" => "($ (* ?m ?a) (* ?m ?b))"),

        rw!("weight-unit"; "(* 1 ?a)" => "?a"),
        rw!("unit-weight"; "?a" => "(* 1 ?a)"),
        rw!("weight-zero"; "(* 0 ?a)" => "0"),
        rw!("weight-commutative"; "(* ?a (* ?b ?c))" => "(* ?b (* ?a ?c))"),
    ]
}

//impl<LinDiff2ws> fn is_greater_than(v: &str, w: &str) -> impl Fn(&mut EGraph2ws, Id, &Subst) -> bool {
//     let v = v.parse().unwrap();
//     let w = w.parse().unwrap();
//     move |egraph, _, subst| {
//         match (&egraph[subst[v]].nodes[0], &egraph[subst[w]].nodes[0]) {
//             (LinDiff2ws::Constant(m), LinDiff2ws::Constant(n)) => m > n,
//             _ => false
//         }
//     }
// }

pub fn rules2_weight_combine<L: Language + Send + Sync + FromOp + 'static, N: Analysis<L> + 'static>() -> Vec<egg::Rewrite<L, N>> {
    vec![
        rw!("constant-mult-commute"; "(c* ?a ?b)" => "(c* ?b ?a)"),
        rw!("constant-add-commute"; "(c+ ?a ?b)" => "(c+ ?b ?a)"),

        rw!("weight-merge"; "(* ?a (* ?b ?z))" => "(* (c* ?a ?b) ?z))"),
        rw!("weight-add"; "($ (* ?m ?a) (* ?n ?a))" => "($ (* (c+ ?m ?n) ?a) 0)"),
        rw!("weight-sub"; "($ (* ?m ?a) (* ?n ($ ?a ?z)))" =>
            "($ (* (c- ?m ?n) ?a) (* ?n ($ z 0)))"
        // if is_greater_than<T>("?m", "?n")
        ),
    ]
}

pub fn rules2_elimination<L: Language + Send + Sync + FromOp + 'static, N: Analysis<L> + 'static>() -> Vec<egg::Rewrite<L, N>> {
    vec![
        rw!("eliminate-add"; "(+ ?a ?b)" => "($ ($ ?a ?b) 0)"),
        rw!("eliminate-integral"; "(I ?a)" => "($ (i ?a) 0)"),
        rw!("sum-invert"; "(- ?a)" => "($ ?a 0)"),
    ]
}

pub fn rules2() -> Vec<egg::Rewrite<LinDiff2X, ()>> {
    let mut rules = rules2_core();
    rules.extend(rules2_shortcuts());
    rules.extend(rules2_elimination());
    rules.extend(rules2_integrate());
    rules.extend(rules2_weights());
    rules.extend(rules2_weight_combine());
    rules
}

egg::test_fn!{
    commutative, rules2(), "($ a b)" => "($ b a)"
}

egg::test_fn! {
    unit_weights, rules2(), "(+ x y)" => "($ ($ (* 1 x) (* 1 y)) 0)"
}

egg::test_fn! {
    unit_weights_neg, rules2(), "(- (+ x y))" => "($ (* 1 x) (* 1 y))"
}

egg::test_fn! {
    cancellation_1, rules2(), "($ (- a) a)" => "0"
}

egg::test_fn! {
    cancellation_2, rules2(), "($ a ($ a c))" => "($ 0 ($ c 0))"
}

egg::test_fn! {
    cancellation_4, rules2(), "(+ (+ x z) (+ w (- x)))" => "($ ($ z w) 0)"
}

egg::test_fn! {
    cancellation_complicated, rules2(), "(+ c (- (+ (- b) (+ a b))))" => "(- ($ (- a) c))"
}

egg::test_fn! {
    unbalanced, rules2(), "(+ a (+ b (+ c d)))" =>
        "($ ($ a b) ($ c d))"
}

egg::test_fn! {
    balance, rules2(), "(+ a (+ b (+ c (+ d (+ e (+ f (+ g h)))))))" =>
        "($ ($ ($ a b) ($ c d)) ($ ($ e f) ($ g h)))"
}
