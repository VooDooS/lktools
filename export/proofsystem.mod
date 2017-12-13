module proofsystem.

% Debugging aids
type announce, spy    o -> o.
type bracket          string -> o -> string -> o.  % Auxiliary

bracket Pre G Post :- print Pre, term_to_string G S, print S, print Post.
announce G :- bracket ">>" G "\n", fail.
spy G :- (bracket ">Entering " G "\n", G, bracket ">Success  " G "\n";
          bracket ">Leaving  " G "\n", fail).

%asyncl A B C   :- announce (asyncl A B C).
%asyncr A B C   :- announce (asyncr A B C).
%syncl  A B C D :- announce (syncl  A B C D).
%syncr  A B C   :- announce (syncr  A B C).

% Negative formulas
checkN (atm N) :- neg_atom N.
checkN (imp A B).

% Positive formulas
checkP (atm N) :- pos_atom N.

% Negative formula or pos atom
checkC (atm N) :- pos_atom N.
checkC A :- checkN A.

% Positive formula or neg atom
checkE A :- checkP A.
checkE (atm N) :- neg_atom N.

%%% Proof System %%%

asyncl (s C) (abs T) (imp A B)      :- pi x\ hyp x A =>  asyncl C (T x) B.                     %% Impr + Sl
asyncl (s C) T E                    :- checkE E, asyncr C T E.                                 %% Sr
asyncr (s C) (app V K) E            :- checkE E, hyp V N, checkN N, syncl C K N E.             %% Dl
asyncr (s C) (up V) P               :- checkP P, syncr C V P.                                  %% Dr

syncl  (s C) (cons V K) (imp A B) E :- checkE E, syncr C V A, syncl C K B E.                   %% Impl
syncl  (s C) (kappa T) P E          :- checkE E, checkP P, pi x\ hyp x P => asyncr C (T x) E.  %% Rl + Sl
syncl  (s C) emp (atm N) (atm N)    :- neg_atom N.                                             %% Il

syncr  (s C) (down T) N             :- checkN N, asyncl C T N.                                 %% Rr
syncr  (s C) V (atm P)              :- hyp V (atm P), pos_atom P.                              %% Ir


substval M K T :- announce (substval M K T ).
applyv A B C :- announce (applyv A B C).
applyc A B C :- announce (applyc A B C).

copy (abs M) (abs N) :- pi x\ copy_val x x => copy (M x) (N x).
copy (app V C) (app W D) :- copy_val V W, copy_cont C D.
copy (up V) (up W) :- copy_val V W.

copy_val (down T)  (down U) :- copy T U.
copy_val succ succ.
copy_val succ2 succ2.
copy_val zero zero.

copy_cont emp emp.
copy_cont (cons V C) (cons W D) :- copy_val V W, copy_cont C D.
copy_cont (kappa M) (kappa N) :- pi x\ copy_val x x => copy (M x) (N x).

subst M K T :- pi x\ copy_cont x K => copy (M x) T.
substval M K T :- pi x\ copy_val x K => copy (M x) T.



apply A B C :- announce (apply A B C).
applyv A B C :- announce (applyv A B C).
applyc A B C :- announce (applyc A B C).

% 
apply Abs (app Val Cont) (app Val Cont') :- applyc Abs Cont Cont'.
apply Abs (up  Val) (Abs Val).
apply Abs (abs B) C :- substval Abs (down (abs B)) C.
%apply Abs (abs B) (C)  :-  pi x\ applyv Abs x (down (B x))   => apply Abs (B x) (C).
%apply Abs (abs B) (abs C)  :- pi x\ applyv Abs x x   => apply Abs (Abs (down (B x))) (C x).

%apply (f\ app f K) (abs B) (app (down (abs B)) K') :- pi x\ applyv Abs x x  => applyc Abs K K'.

%apply (f\ app f (cons X (kappa K))) (abs B) (K (down (B X))) :- apply (x\ K x) (abs B) R

% apply (x\ app (V x) (C x)) (abs B) 
% apply (x\ abs )
% apply (x\ up )


applyc Abs (cons Val K) (cons Val' K') :- applyv Abs Val Val', applyc Abs K K'.

applyc Abs emp emp.
applyc Abs (kappa K)    (kappa K')     :- pi x\ applyv Abs x x => apply Abs (K x) (K' x).

applyv Abs (down T) (down T') :- apply Abs T T'.
applyv Abs zero zero.
applyv Abs succ succ.
applyv Abs succ2 succ2.

% papply  (x \ app succ (cons x (kappa k\ up k)))  (app succ (cons zero (kappa k\ up k))) Ans.
% papply  (x \ app succ (cons x (kappa k\ up k)))  (app succ (cons zero (kappa k\ app succ (cons k (kappa j\ up j))))) Ans.

% (abs f. f zero) (abs x. s x)  --> s z
% papplyv  (f \ app f (cons zero (kappa up)))  (down (abs x\ app succ (cons x (kappa up)))) Ans.

%% Pretty printer %

pretty (abs T) I S :- SX is "x" ^ (int_to_string I), 
       	       	      pi x\ prettyv x _ SX => (
       	       	      	 I2 is I + 1, pretty (T x) I2 ST, 
       	       	      	  	 S is "λ" ^ SX  ^ ".(" ^ ST ^ ")" ).
pretty (app V K) I S :- prettyv V I SV, prettyk K I SK,
       	      	      S is SV ^ " " ^ SK.
pretty (up V) I S :- prettyv V I SV, S is "↑(" ^ SV ^ ")".

prettyv (down T) I S :- pretty T I ST, S is "↓(" ^ ST ^ ")".
prettyv zero _ "0".
prettyv succ _ "s".
prettyv succ2 _ "s2".

prettyk emp I "ε".
prettyk (cons V T) I S :- prettyv V I SV, prettyk T I SK, S is SV ^ "::" ^ SK.
prettyk (kappa K) I S :-  SV is "k" ^ (int_to_string I), 
	       	      	  pi k\ prettyv k _ SV => ( 
	       	      	     I2 is I + 1, pretty (K k) I2 SK, 
	       	       	     	    S is "κ" ^ SV ^ ".(" ^ SK ^ ")" ).

papply VT T S :- pretty (abs VT) 0 S1, pretty T 0 S2, print S1, print "\n", print S2, print "\n\n", apply VT T A, pretty A 0 S.
papplyv VT T S :- pretty (abs VT) 0 S1, prettyv T 0 S2, print S1, print "\n", print S2, print "\n\n", applyv VT T A, prettyv A 0 S.
papplyc VT T S :- pretty (abs VT) 0 S1, prettyk T 0 S2, print S1, print "\n", print S2, print "\n\n", applyc VT T A, prettyk A 0 S.