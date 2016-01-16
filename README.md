# Template Lambda Calculus

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

<mark>XXX</mark>: Why while we have already use `substIn` to reduce the application, we still put the abstraction variable's binding to the environment? It is because, when we are using `substIn`, only compile-time structure will be inspected. Thus, the `$i` in brackets is ignored. So, only with this method, can binding be passed from outer-splicing scope to inner-splicing scope. <mark>Nevertheless, I think it is not an elegant solution</mark>.

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


### Stage safety

## References

* <cite id="ref-1"> Sheard, T., & Jones, S. P. (2002). Template meta-programming for Haskell. ACM SIGPLAN Notices, 37(12), 60. http://doi.org/10.1145/636517.636528 </cite>

