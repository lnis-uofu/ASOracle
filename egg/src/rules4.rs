use egg::{rewrite as rw, *};

define_language! {
    pub enum LinDiff4NW {
        "i" = Integrate(Id),
        "$" = Summing([Id; 4]),
        Constant(u32),
        Symbol(Symbol),
    }
}

define_language! {
    pub enum LinDiff4 {
        "i" = Integrate(Id),
        "$" = Summing([Id; 4]),
        "*" = Weight([Id; 2]),
        Constant(u32),
        Symbol(Symbol),
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

pub struct LinDiff2CostFn;
impl egg::CostFunction<LinDiff2s> for LinDiff2sCostFn {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &LinDiff2s, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            LinDiff2s::Symbol(..) => 0,
            LinDiff2s::Constant(..) => 0,
            _ => 1,
        };
        enode.fold(op_cost, |sum, i| sum + costs(i))
    }
}

pub struct LinDiff4CostFn;
impl egg::CostFunction<LinDiff4> for LinDiff2WCostFn {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &LinDiff2W, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            LinDiff2W::Weight(..) => 0,
            LinDiff2W::Symbol(..) => 0,
            LinDiff2W::Constant(..) => 0,
            _ => 1,
        };
        enode.fold(op_cost, |sum, i| sum + costs(i))
    }
}

// pub struct LinDiff2wsCostFn;
// impl egg::CostFunction<LinDiff2ws> for LinDiff2wsCostFn {
//     type Cost = usize;
//     fn cost<C>(&mut self, enode: &LinDiff2ws, mut costs: C) -> Self::Cost
//     where
//         C: FnMut(Id) -> Self::Cost,
//     {
//         let op_cost = match enode {
//             LinDiff2ws::Add(..) => 10000,
//             LinDiff2ws::Integrate(..) => 10000,
//             LinDiff2ws::Mult(..) => 10000,
//             LinDiff2ws::Symbol(..) => 0,
//             LinDiff2ws::ConstMult(..) => 0,
//             LinDiff2ws::ConstAdd(..) => 0,
//             LinDiff2ws::ConstSub(..) => 0,
//             LinDiff2ws::Constant(..) => 0,
//             LinDiff2ws::Weight(..) => 0,
//             _ => 1,
//         };
//         enode.fold(op_cost, |sum, i| sum + costs(i))
//     }
// }

pub fn rules4_integrate<T: Language>() -> Vec<egg::Rewrite<T, ()>> {
    vec![
        rw!("integrate-split"; "(i ($ ?a ?b))" => "($ (i ?a) (i ?b))"),
        rw!("integrate-join"; "($ (i ?a) (i ?b))" => "(i ($ ?a ?b))"),
        rw!("integrate-zero"; "(i 0)" => "0"),
    ]
}

pub fn rules4_core<T: Language>() -> Vec<egg::Rewrite<T, ()>> {
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

pub fn rules4_shortcuts<T: Language>() -> Vec<egg::Rewrite<T, ()>> {
    vec![
        // would be implicit from swap and neg-zero
        rw!("factor-neg"; "($ ($ ?a 0) ($ ?b 0) ?c ?d)" => "($ ($ ?a ?b) 0 ?c ?d)"),
        // would be implicit from zero-neg and swap
        rw!("distrib-neg"; "($ ($ ?a ?b) 0 ?c ?d)" => "($ ($ ?a 0) ($ ?b 0) ?c ?d)"),
        // Would be implicit from double-neg swap double-neg
        rw!("cancel-add"; "($ ($ ?a 0 0 0) ?a ?b ?c) " => "($ 0 0 ?c ?d)"),
        // implicit from neg-double and swap
        rw!("push-in"; "($ ?a ($ ?b ?x ?y ?z) ?v ?w)" =>
            "($ ($ ?b 0 0 0) ($ ($ ?a 0) ?x ?y ?z)) ?v ?w)"),
    ]
}

pub fn rules4_weights<T: Language>() -> Vec<egg::Rewrite<T, ()>> {
    vec![
        // Weights
        rw!("factor-integrate"; "(i (w ?a ?b))" => "(w ?a (i ?b))"),
        rw!("distribute-integrate"; "(w ?a (i ?b))" => "(i (w ?a ?b))"),

        rw!("factor-weight"; "($ (w ?m ?a) (w ?m ?b) (w ?m ?c) (w ?m ?d))" =>
            "(w ?m ($ ?a ?b ?c ?d))"),
        rw!("distribute-weight"; "(w ?m ($ ?a ?b ?c ?d))" =>
            "($ (w ?m ?a) (w ?m ?b) (w ?m ?c) (w ?m ?d))"),

        rw!("weight-unit"; "(w 1 ?a)" => "?a"),
        rw!("unit-weight"; "?a" => "(w 1 ?a)"),
        rw!("weight-zero"; "(w 0 ?a)" => "0"),
        rw!("weight-commutative"; "(w ?a (w ?b ?c))" => "(w ?b (w ?a ?c))"),
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

pub fn rules4_weight_combine<T: Language>() -> Vec<egg::Rewrite<T, ()>> {
    vec![
        rw!("constant-mult-commute"; "(c* ?a ?b)" => "(c* ?b ?a)"),
        rw!("constant-add-commute"; "(c+ ?a ?b)" => "(c+ ?b ?a)"),

        rw!("weight-merge"; "(w ?a (w ?b ?z))" => "(w (c* ?a ?b) ?z))"),
        rw!("weight-add"; "($ (w ?m ?a) (w ?n ?a))" => "($ (w (c+ ?m ?n) ?a) 0"),
        rw!("weight-sub"; "($ (w ?m ?a) (w ?n ($ ?a ?z)))" =>
            "($ (w (c- ?m ?n) ?a) (w ?n ($ z 0)))"
        // if is_greater_than<T>("?m", "?n")
        ),
    ]
}

pub fn rules4_elimination<T: Language>() -> Vec<egg::Rewrite<T; ()> {
    vec![
        rw!("eliminate-add"; "(+ ?a ?b)" => "($ ($ ?a ?b) 0 0 0)"),
        rw!("eliminate-integral"; "(I ?a)" => "($ (i ?a) 0 0 0)"),
        rw!("eliminate-neg"; "(- ?a)" => "($ ?a 0 0 0)"),
        rw!("eliminate-mult"; "(x ?a ?b)" => "(* ?a ?b)"),
    ]
}


egg::test_fn!{
    commutative, rules4(), "($ a b c d)" => "($ d c b a)"
}

egg::test_fn! {
    unit_weights, rules4(), "(+ x y)" => "(- ($ (w 1 x) (w 1 y) 0 0))"
}

egg::test_fn! {
    unit_weights_neg, rules4(), "(- (+ x y))" => "($ (w 1 x) (w 1 y) 0 0)"
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
    cancellation_4, rules4(), "(+ (+ x z) (+ w (- x)))" => "(- ($ z w 0 0))"
}

egg::test_fn! {
    cancellation_complicated, rules4(), "(+ c (- (+ (- b) (+ a b))))" => "(- ($ (- a) c 0 0))"
}

egg::test_fn! {
    unbalanced, rules4(), "(+ a (+ b (+ c (+ d (+ e (+ f g))))))" =>
        "(- ($ c (- ($ f g d e)) a b))"
}
