use egg::{rewrite as rw, *};

define_language! {
    pub enum LinDiff2NW {
        "i" = Integrate(Id),
        "$" = Summing([Id; 2]),
        Constant(u32),
        Symbol(Symbol),
    }
}

define_language! {
    pub enum LinDiff2 {
        "i" = Integrate(Id),
        "$" = Summing([Id; 2]),
        "*" = Weight([Id; 2]),
        Constant(u32),
        Symbol(Symbol),
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

pub fn rules2_integrate<T: Language>() -> Vec<egg::Rewrite<T, ()>> {
    vec![
        rw!("integrate-split"; "(i ($ ?a ?b))" => "($ (i ?a) (i ?b))"),
        rw!("integrate-join"; "($ (i ?a) (i ?b))" => "(i ($ ?a ?b))"),
        rw!("integrate-zero"; "(i 0)" => "0"),
    ]
}

pub fn rules2_core<T: Language>() -> Vec<egg::Rewrite<T, ()>> {
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

pub fn rules2_shortcuts<T: Language>() -> Vec<egg::Rewrite<T, ()>> {
    vec![
        // would be implicit from swap and neg-zero
        rw!("factor-neg"; "($ ($ ?a 0) ($ ?b 0))" => "($ ($ ?a ?b) 0)"),
        // would be implicit from zero-neg and swap
        rw!("distrib-neg"; "($ ($ ?a ?b) 0)" => "($ ($ ?a 0) ($ ?b 0))"),
        // Would be implicit from double-neg swap double-neg
        rw!("cancel-add"; "($ ($ ?a ?c) ?a)" => "?c"),
        // implicit from neg-double and swap
        rw!("push-in"; "($ ?a ($ ?b ?c))" => "($ ($ ?b 0) ($ ($ ?a 0) ?d))"),
    ]
}

pub fn rules2_weights<T: Language>() -> Vec<egg::Rewrite<T, ()>> {
    vec![
        // Weights
        rw!("factor-integrate"; "(i (w ?a ?b))" => "(w ?a (i ?b))"),
        rw!("distribute-integrate"; "(w ?a (i ?b))" => "(i (w ?a ?b))"),

        rw!("factor-weight"; "($ (w ?m ?a) (w ?m ?b))" => "(w ?m ($ ?a ?b))"),
        rw!("distribute-weight"; "(w ?m ($ ?a ?b))" => "($ (w ?m ?a) (w ?m ?b))"),

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

pub fn rules2_weight_combine<T: Language>() -> Vec<egg::Rewrite<T, ()>> {
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

pub fn rules2_elimination<T: Language>() -> Vec<egg::Rewrite<T; ()> {
    vec![
        rw!("eliminate-add"; "(+ ?a ?b)" => "($ ($ ?a ?b) 0)"),
        rw!("eliminate-integral"; "(I ?a)" => "($ (i ?a) 0)"),
        rw!("sum-invert"; "(- ?a)"  "($ ?a 0)"),
    ]
}
