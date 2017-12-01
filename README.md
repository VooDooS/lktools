# A parser for LambdaKappa-calculus using menhir and OCaml

## Basic install
- Make sure you have `ocaml-4.02.3` and `menhir` installed, best way to do that is to use `opam`, the OCaml package manager.
- Clone or download the repository.
- Invoke `make` to build the project

## The grammar of the calculus :

```ocaml
type tm =
  | App of value  * cont
  | Abs of (value -> tm)
  | Up of value
and cont =
  | Empty
  | Kappa of (value -> tm)
  | Cons of value * cont
and value =
  | Ident of string
  | Down of tm
```

## Possible syntaxes :


