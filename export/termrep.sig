sig termrep.
accum_sig proofsystem.

% Counter in proofsystem.sig
%kind ct type.
%type z ct.
%type s ct -> ct.

% Different forms of natural numbers as lambda-terms
type neg_numeral   ct -> tm -> o.
type pos_numeral   ct -> tm -> o.

% Different forms of binary trees as lambda-terms
type neg_tree      ct -> tm -> o.
type pos_tree      ct -> tm -> o.

type nat, o                          atom.
type forall, implies, or, equal      val.
type ++, **, zero, succ              val.

% Conditional branch (non-logical)
type if               o -> o -> o -> o.

% Predicates for doing testing
type example  	      int -> tm -> fm -> o.
type test     	      int -> o.
type test_all 	      o.

