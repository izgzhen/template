# Implementation of Template Lambda Calculus

This implementation is based on theory behind Template Haskell [[1]](#ref-1).

After about 6 hours' hacking, I finally get something working on some basic examples. I have to commit that it is very difficult -- I was almost writing code *only by intuition*! Next, I am going to go over my implementation, and maybe a more formal proof about its correctness.

## Basics
The lambda calculus is the simply typed one. No polymorphism, not even "let" binding. The focus is on the template system. It is *very* similar to the Haskell one, for the following characteristics:

1. Splice operator `$`: template expansion at call-site
2. Quasi-Quote `[| ... |]`
3. Type-safety inside template
4. Syntax-construction functions, including `genstr`, `TmAbs`, `TyInt` et cetera

The examples in `src/Example.hs`:

```haskell
-- tm1
$((\(s : Int -> TyQ) -> (s 1)) (\(i: Int) -> [| $i + $i |]))

-- tm2
$((\(s : String) -> TmAbs s TyInt (TyVar s)) genstr)
```

## Inner Working
I only implement the stage-1, i.e. type checking before expansion and the expansion. However, the algorithms in stage-1 can be easily adopted as stage-2 type checking or interpreting of common code.

The stage means that:
        
        _______ with meta function ____ ___________ common code _______
        |                              |                              |
    typeCheckStage1 term  ->  compileStage1 term -> typeCheckStage2 term -> Backend

In stage 1, the type checking of splice will always return `TyBottom`, which means that we don't know its type until it is expanded. However, the meta-expression inside splice must be typed as `tyQ` itself. The main source of `TyQ` is quasi-quoting and syntax-construction functions.

The `compileStage1` (i.e. `compile` in `src/Compiler.hs` is very much like a state machine. The *Compile* stage is simply going through the code and find meta call-site. When entering the meta call-site, it changes to *Evaluate* stage (i.e. `evaluate`), which will try to evaluate the meta-function. If then it encounters bracket expression, it will change to *Compile* stage again; while if it encounters `TmTm` expression (i.e. syntax-construction expression in user-level), it will go to `evaluateTm`.

The compile time environment is composed of a name generator and a name to expression binding. The bindings is used to store built-in functions (i.e. `stdlib`) which are mainly syntax-construction functions, as well as compile-time variable bindings.

<mark>XXX</mark>: Why while we have already use `substIn` to reduce the application, we still put the abstraction variable's binding to the environment? It is because, when we are using `substIn`, only compile-time structure will be inspected. Thus, the `$i` in brackets is ignored. So, only with this method, can binding be passed from outer-splicing scope to inner-splicing scope. <mark>XXX</mark>: Nevertheless, I think it is not an elegant solution.

## Correctness
The first problem is *how to define "correctness" here*?

* The compile-time semantics model
* If the stage 1 type checking is passed, then their should be no stage 1 compile-time exception.
* No stage 1 exclusive stuff will still exist after expansion

### Semantics model
$Comp: (Env, Term) \mapsto Term$ is the expansion algorithm.

$$\frac{}{\Gamma; x \mapsto_{comp} x}\text{COMP-VAR}$$
$$\frac{}{\Gamma; i \mapsto_{comp} i}\text{COMP-INT}$$
$$\frac{}{\Gamma; s \mapsto_{comp} s}\text{COMP-STR}$$

$$\frac{\Gamma; t_1 \mapsto_{comp} t_1' \quad
        \Gamma; t_2 \mapsto_{comp} t_2'}
       {\Gamma; t_1 \, t_2 \mapsto_{comp} t_1' \, t_2'}\text{COMP-APP}$$

$$\frac{\Gamma; tm \mapsto_{comp} tm'}{\Gamma; \lambda x : T, tm \mapsto_{comp} \lambda x : T, tm'}\text{COMP-ABS}$$

$$\frac{tm \mapsto_{eval} tm'}{$tm \mapsto_{comp} tm'}\text{COMP-EVAL}$$

$$\frac{}{\Gamma; [ tm ] \mapsto_{comp} \bot}\text{COMP-BRACKET}$$
$$\frac{}{\Gamma; TmTm \mapsto_{comp} \bot}\text{COMP-TMTERM}$$
$$\frac{}{\Gamma; TmType \mapsto_{comp} \bot}\text{COMP-TMTYPE}$$

$Eval: (Env, Term) \mapsto Term$ is a compile-time reduction algorithm.

$$\frac{\Gamma; t_1 \mapsto_{eval} \lambda x : T. t_{12} \quad
        \Gamma; t_2 \mapsto_{eval} t_2' \quad
        \Gamma, x \mapsto t_2'; t_{12}[t_2'/x] \mapsto_{eval} t_{12}'}
       {\Gamma; t_1 \, t_2 \mapsto_{eval} t_{12}'}\text{EVAL-APP}$$

$$\frac{\Gamma; tm \mapsto_{comp} tm'}{\Gamma; [tm] \mapsto_{eval} [tm']}\text{EVAL-BRACKET}$$
$$\frac{}{\Gamma; $tm \mapsto_{eval} \bot}\text{EVAL-SPLICE}$$
$$\frac{\Gamma \vdash x \mapsto tm \quad
        tm \mapsto_{eval}tm'}{\Gamma; x \mapsto_{eval} tm'}\text{EVAL-VAR}$$
$$\frac{\Gamma; tmt \in TmTerm \mapsto_{evalTm} tm'}
       {\Gamma; TmTm(tmt) \mapsto_{eval}[tm']}\text{EVAL-TMTERM}$$

$$\frac{}{\Gamma; i \mapsto_{eval} i}\text{EVAL-INT}$$
$$\frac{}{\Gamma; s \mapsto_{eval} s}\text{EVAL-STR}$$
$$\frac{}{\Gamma; \lambda x :T. tm \mapsto_{eval} \lambda x :T. tm}\text{EVAL-INT}$$
$$\frac{}{\Gamma; TmType(ty) \mapsto_{eval} TmType(ty)}\text{EVAL-TMTYPE}$$

$EvalTm: (Env, TmTerm) \mapsto Term$ is a syntax construct mappinge algorithm.

$$\frac{tm \mapsto_{eval} i}{TmTmInt(tm) \mapsto_{evalTm} i}\text{EVALTM-INT}$$
$$\frac{tm \mapsto_{eval} s}{TmTmString(tm) \mapsto_{evalTm} s}\text{EVALTM-STR}$$
$$\frac{tm \mapsto_{eval} s \quad
        x = Var(s)}{TmTmInt(tm) \mapsto_{evalTm} x}\text{EVALTM-VAR}$$
$$\frac{t_1 \mapsto_{eval} t_1' \quad
        t_2 \mapsto_{eval} t_2'}
       {TmTmApp(t_1, t_2) \mapsto_{evalTm}t_1' \, t_2' }\text{EVALTM-APP}$$
$$\frac{t_1 \mapsto_{eval} s \quad
        t_2 \mapsto_{eval} T \quad
        t_3 \mapsto_{eval} t_3'}
       {TmTmAbs(t_1, t_2, t_3) \mapsto_{evalTm} \lambda s : T. t_3'}\text{EVALTM-ABS}$$

### Type safety
The type safety can be categorized into two aspects:

* Common ones
* Meta ones

All the divergent cases stated as above (with $\bot$) are belonging to the second one. The common ones are the classical ones for simply typed lambda calculus.

Like the compilation, the type checking also endows contexts. However, for the common cases, typeChecking code can be shared (see `typeCheck`). For meta-specific constructs, type checker behaves differently (see `typeCheckBracket` and `typeCheckSplice`).

So, these rules effective eliminates the meta undefined behaviours:

```haskell
typeCheckBracket env (TmBracket _) = Left "TmBracket in bracket"
typeCheckBracket env (TmType _)    = Left "TmType in bracket"
typeCheckBracket env (TmTm _)      = Left "TmTm in bracket"
typeCheckSplice env (TmSplice tm)  = Left "TmSplice in splice"
```

One thing worth noting is that, the splice term inside bracket is typed as `TyWildCard`, meaning a type what can match anything. However, after expansion, it should not appear again. And to well-type the function application under the presence of such type, I come up with a hackish `unify` function (Hope the algorithm is correct :P).

Since the type system is pretty naïve, I will not do any formalization here.

### Stage safety
> Theorem #1: For any term input $t$, after the stage 1 compilation, it should have no bracket, splice, `TmTerm` or `TmType` (i.e., is a *common* term).

Proof: Let's look at $Comp$ rules, for COMP-APP, COMP-ABS, it can be inductively proven. For COMP-BRACKET, COMP-TMTERM, COMP-TMTYPE, they should not appear after type checking. For COMP-INT, COMP-STR, and COMP-VAR, it is the trivial case.

Now let's see the non-trivial case: COMP-EVAL.

> Theorem #2: For any term input splice $$t$, after stage 1 evaluation of $t$, it will become common term $t'$.

Proof: EVAL-APP, EVAL-VAR can be proven inductively; EVAL-STR, EVAL-INT, EVAL-TMTYPE are trivial cases; EVAL-SPLICE is eliminated by typing rules; EVAL-BRACKET can be proven using the theorem #1 (<mark>XXX</mark>: mutually recursive); EVAL-TMTERM will be proven later; EVAL-ABS can be reduced to substitution, which can be proven inductively.

> Theorem #3: For any `TmTerm` $tmt$, after `evalTm`, it will become common term $t'$.

Proof: EVALTM-VAR, EVALTM-VAR and EVALTM-STR are trivial cases; Other two can be proven inductively.




## References

* <cite id="ref-1"> Sheard, T., & Jones, S. P. (2002). Template meta-programming for Haskell. ACM SIGPLAN Notices, 37(12), 60. http://doi.org/10.1145/636517.636528 </cite>

