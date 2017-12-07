# A toolset for lkterms

## Basic usage
- Make sure you have `ocaml-4.02.3` and `menhir` installed, best way to do that is to use `opam`, the OCaml package manager. You also need the teyjus λProlog interpreter to run the exported λProlog module.
- Clone or download the repository.
- Invoke `make run` from the root of the repository to build the project and run the executable.
- Change directory to `export/` and invoke `make run` to compile λProlog and run the toplevel.

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


## File organization :
- `ast.ml` : contains the target type for the parsing of λ-expressions, a tool to bind vars in a term and a printer.
- `lk.ml` : contains the type of λκ-terms, a tool to bind vars in a term and a printer.
- `lktools.ml` : contains an equality check for λκ-terms and a function tu build a λκ-term from a λ-term.
- `export.ml` contains a function to output λκ-terms to a module file readable by λProlog. Output of the export is put in the `export/` directory.
- `examples.txt` contians some lambda-terms to test the program on.
- `main.ml` parses the example file and output terms to the terminal and a λProlog module.

## Accepted source syntaxes for lambda-terms :


## References :
Nannestad, Guenot, Gustafsson : Computation in Focused Intuitionistic Logic ([HAL](https://hal.archives-ouvertes.fr/hal-01249216/))

Gérard, Miller : Separating Functional Computation from Relations ([HAL](https://hal.inria.fr/hal-01615683)) 
