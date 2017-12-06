# A toolset for lkterms

## Basic install
- Make sure you have `ocaml-4.02.3` and `menhir` installed, best way to do that is to use `opam`, the OCaml package manager.
- Clone or download the repository.
- Invoke `make` to build the project

## The grammar of the LK-calculus :

```ocaml
type tm =
  | App of value  * cont
  | Abs of string * (value -> tm)
  | Up of value
and cont =
  | Empty
  | Kappa of string * (value -> tm)
  | Cons of value * cont
and value =
  | Ident of string
  | Down of tm
```

## Accepted source syntaxes for lambda-terms :


## References :
Nannestad, Guenot,Gustafsson : Computation in Focused Intuitionistic Logic ([HAL](https://hal.archives-ouvertes.fr/hal-01249216/))

Gérard, Miller : Separating Functional Computation from Relations ([HAL](https://hal.inria.fr/hal-01615683)) 
