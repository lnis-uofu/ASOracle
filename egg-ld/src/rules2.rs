use egg::{rewrite as rw, *};

pub type RecExpr = egg::RecExpr<LinDiff2Synth>;
pub type Pattern = egg::Pattern<LinDiff2Synth>;
pub type EGraph = egg::EGraph<LinDiff2Synth, ()>;
pub type Rewrite = egg::Rewrite<LinDiff2Synth, ()>;
pub type Runner = egg::Runner<LinDiff2Synth, (), IterData>;
pub type Iteration = egg::Iteration<IterData>;

define_language! {
    pub enum LinDiff2 {
        "&" = Integrate(Id),
        "$" = Summing([Id; 2]),
        "*" = Weight([Id; 2]),
        Constant(i32),
        Symbol(Symbol),
    }
}

pub struct IterData {
    pub extracted: Vec<(Id, Extracted)>,
}

pub struct Extracted {
    pub best: RecExpr,
    pub cost: usize,
}

impl IterationData<LinDiff2Synth, ()> for IterData {
    fn make(runner: &Runner) -> Self {
        let extractor = Extractor::new(&runner.egraph, LinDiff2SynthCostFn);
        let extracted = runner
            .roots
            .iter()
            .map(|&root| {
                let (cost, best) = extractor.find_best(root);
                let ext = Extracted { cost, best };
                (root, ext)
            })
            .collect();
        Self { extracted }
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
    pub enum LinDiff2Synth {
        "out8" = Output8([Id; 8]),
        "out7" = Output7([Id; 7]),
        "out6" = Output6([Id; 6]),
        "out5" = Output5([Id; 5]),
        "out4" = Output4([Id; 4]),
        "out3" = Output3([Id; 3]),
        "out2" = Output2([Id; 2]),
        "out1" = Output1([Id; 1]),
        "e+" = Add([Id; 2]),
        "e*" = Mult([Id; 2]),
        "e&" = Integrate(Id),
        "e-" = Negative(Id),
        "$" = Summing([Id; 2]),
        "&" = Integrating(Id),
        "*" = Weight([Id; 2]),

        "c*" = ConstMult([Id; 2]),
        "c+" = ConstAdd([Id; 2]),
        "c-" = ConstSub([Id; 2]),
        Constant(u32),
        Symbol(egg::Symbol),
    }
}

pub struct LinDiff2SynthCostFn;
impl egg::CostFunction<LinDiff2Synth> for LinDiff2SynthCostFn {
    type Cost = usize;
    fn cost<C>(&mut self, enode: &LinDiff2Synth, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let op_cost = match enode {
            LinDiff2Synth::Add(..) => 10000,
            LinDiff2Synth::Integrate(..) => 10000,
            LinDiff2Synth::Mult(..) => 10000,
            LinDiff2Synth::Negative(..) => 10000,
            LinDiff2Synth::Summing(..) => 1,
            LinDiff2Synth::Integrating(..) => 1,
            _ => 0,
        };
        enode.fold(op_cost, |sum, i| sum + costs(i))
    }
}

pub fn rules2_integrate<L: Language + Send + Sync + FromOp + 'static, N: Analysis<L> + 'static>() -> Vec<egg::Rewrite<L, N>> {
    vec![
        rw!("integrate-split"; "(& ($ ?a ?b))" => "($ (& ?a) (& ?b))"),
        rw!("integrate-join"; "($ (& ?a) (& ?b))" => "(& ($ ?a ?b))"),
        rw!("integrate-zero"; "(& 0)" => "0"),
        rw!("integrate-factor-neg"; "($ (& ($ ?a 0)) 0)" => "(& ?a)"),
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
        rw!("factor-integrate"; "(& (* ?a ?b))" => "(* ?a (& ?b))"),
        rw!("distribute-integrate"; "(* ?a (& ?b))" => "(& (* ?a ?b))"),

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
        rw!("eliminate-add"; "(e+ ?a ?b)" => "($ ($ ?a ?b) 0)"),
        rw!("eliminate-mult"; "(e* ?a ?b)" => "(* ?a ?b)"),
        rw!("eliminate-integral"; "(e& ?a)" => "($ (& ?a) 0)"),
        rw!("sum-invert"; "(e- ?a)" => "($ ?a 0)"),
    ]
}

pub fn rules2() -> Vec<egg::Rewrite<LinDiff2Synth, ()>> {
    let mut rules = rules2_core();
    rules.extend(rules2_shortcuts());
    rules.extend(rules2_elimination());
    rules.extend(rules2_integrate());
    // rules.extend(rules2_weights());
    // rules.extend(rules2_weight_combine());
    rules
}

egg::test_fn!{
    commutative, rules2(), "($ a b)" => "($ b a)"
}

egg::test_fn! {
    unit_weights, rules2(), "(e+ x y)" => "($ ($ (* 1 x) (* 1 y)) 0)"
}

egg::test_fn! {
    unit_weights_neg, rules2(), "(e- (e+ x y))" => "($ (* 1 x) (* 1 y))"
}

egg::test_fn! {
    cancellation_1, rules2(), "($ (e- a) a)" => "0"
}

egg::test_fn! {
    cancellation_2, rules2(), "($ a ($ a c))" => "($ 0 ($ c 0))"
}

egg::test_fn! {
    cancellation_4, rules2(), "(e+ (e+ x z) (e+ w (e- x)))" => "($ ($ z w) 0)"
}

egg::test_fn! {
    cancellation_complicated, rules2(), "(e+ c (e- (e+ (e- b) (e+ a b))))" => "(e- ($ (e- a) c))"
}

egg::test_fn! {
    unbalanced, rules2(), "(e+ a (e+ b (e+ c d)))" =>
        "($ ($ a b) ($ c d))"
}

egg::test_fn! {
    balance, rules2(), "(e+ a (e+ b (e+ c (e+ d (e+ e (e+ f (e+ g h)))))))" =>
        "($ ($ ($ a b) ($ c d)) ($ ($ e f) ($ g h)))"
}
