sig proofsystem.

% Formulas
kind fm, atom     type.
type p, q, n, m   atom.
type atm          atom -> fm.
type imp          fm -> fm -> fm.

% Terms
kind tm, val, cont  type.
type abs    (val -> tm) -> tm.
type app    val -> cont -> tm.
type up     val -> tm.
type down   tm -> val.
type emp    cont.
type cons   val -> cont -> cont.
type kappa  (val -> tm) -> cont.

% Proof System auxillary predicates
type hyp                         val -> fm -> o.
type pos_atom, neg_atom               atom -> o.
type checkN, checkP, checkC, checkE     fm -> o.

% Counter
kind ct type.
type z ct.
type s ct -> ct.

% Sequents
type asyncl ct -> tm   -> fm -> o.
type asyncr ct -> tm   -> fm -> o.
type syncl  ct -> cont -> fm -> fm -> o.
type syncr  ct ->  val -> fm -> o.

type copy tm -> tm -> o.
type copy_val val -> val -> o.
type copy_cont cont -> cont -> o.

type subst       (cont -> tm) -> cont -> tm -> o.
type substval    (val -> tm) -> val -> tm -> o.

type apply       (val -> tm) -> tm -> tm -> o.
type applyc      (val -> tm) -> cont -> cont -> o.
type applyv      (val -> tm) -> val  -> val  -> o.


type zero, succ, succ2          val.


% Pretty print %
type pretty tm -> int -> string -> o.
type prettyv val -> int -> string -> o.
type prettyvar val -> string -> o.
type prettyk cont -> int -> string -> o.

% Pretty subst %
type papply (val -> tm) -> tm -> string -> o.
type papplyv (val -> tm) -> val -> string -> o.
type papplyc (val -> tm) -> cont -> string -> o.
