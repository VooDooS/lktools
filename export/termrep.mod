module termrep.
accumulate proofsystem.

if P Q R :- P, !, Q.
if P Q R :- R.

pos_atom p & pos_atom q.
neg_atom n & neg_atom m.

% The usual Church numerals as lambda-terms.
neg_numeral z     (abs zero\ abs succ\ app zero emp).
neg_numeral (s C) (abs zero\ abs succ\ app succ (cons (down (Num zero succ)) emp)) :-
  neg_numeral  C  (abs zero\ abs succ\ Num zero succ).

% Another form of numeral as lambda-terms: nested naming.
pos_numeral z     (abs zero\ abs succ\ up zero).
pos_numeral (s C) (abs zero\ abs succ\ app succ (cons zero (kappa w\ Num w succ))) :-
  pos_numeral  C  (abs zero\ abs succ\ Num zero succ).

% Balanced binary trees using a negative bias atom
neg_tree    z  (abs a\ abs f\ app a emp).
neg_tree (s C) (abs a\ abs f\ app f (cons (down (Tree a f)) (cons (down (Tree a f)) emp))) :- 
  neg_tree C (abs a\ abs f\ Tree a f).

% Balanced binary trees using a positive bias atom
pos_tree    z  (abs a\ abs f\ up a).
pos_tree (s C) (abs a\ abs f\ app f (cons a (cons a (kappa x\ Tree x f)))) :-
  pos_tree C (abs a\ abs f\ Tree a f).

% Predicates for testing 
test N   :- example N Tm Fm, asyncl C Tm Fm.
test_all :- example N Tm Fm, term_to_string N Str, print Str, 
            if (asyncl C Tm Fm) (print " Yes\n") (print " No\n"), fail.
test_all.

%%% Test cases
example 1 (abs x\ app x emp)           (imp (atm n) (atm n)).
example 2 (abs x1\ abs x2\ app x2 emp) (imp (atm n) (imp (atm n) (atm n))).
example 3 (abs x1\ abs x2\ app x1 emp) (imp (atm n) (imp (atm n) (atm n))).
example 4 (abs x1\ abs x2\ up x1)      (imp (atm p) (imp (imp (atm p) (atm p)) (atm p))).
example 5 (abs x1\ abs x2\ app x2 (cons x1 emp)) (imp (atm p) (imp (imp (atm p) (atm n)) (atm n))).
example 6 (abs x\ abs y\ app y (cons (down (app x emp)) (kappa z\ up z))) (imp (atm n) (imp (imp (atm n) (atm p)) (atm p))).


% Church numerals
example 11 (abs zero\ abs succ\ app zero emp)
           (imp (atm n) (imp (imp (atm n) (atm n)) (atm n))).
example 12 (abs zero\ abs succ\ app succ (cons (down (app zero emp)) emp)) 
           (imp (atm n) (imp (imp (atm n) (atm n)) (atm n))).
example 13 (abs zero\ abs succ\ app succ (cons (down (
                                app succ (cons (down (app zero emp)) emp))) emp))
           (imp (atm n) (imp (imp (atm n) (atm n)) (atm n))).
example 14 Num (imp (atm n) (imp (imp (atm n) (atm n)) (atm n))) :-
  neg_numeral (s (s (s (s (s (s z)))))) Num.
% Switch to positive atoms
example 21 (abs x\ up x) (imp (atm p) (atm p)).
example 22 (abs x1\ abs x2\ up x2) (imp (atm p) (imp (atm p) (atm p))).
example 23 (abs x1\ abs x2\ up x1) (imp (atm p) (imp (atm p) (atm p))).
% A different form of number based on a positive atom
example 31 (abs zero\ abs (succ\ up zero))
           (imp (atm p) (imp (imp (atm p) (atm p)) (atm p))).
% lambda zero lambda succ. name w = (succ zero) in w
example 32 (abs zero\ abs succ\ app succ (cons zero (kappa w\ up w)))
           (imp (atm p) (imp (imp (atm p) (atm p)) (atm p))).
% lambda zero lambda succ. name w = (succ zero) in zero
example 33 (abs zero\ abs succ\ app succ (cons zero (kappa w\ up zero)))
           (imp (atm p) (imp (imp (atm p) (atm p)) (atm p))).
example 34 Num (imp (atm p) (imp (imp (atm p) (atm p)) (atm p))) :-
  pos_numeral (s (s (s (s (s (s z)))))) Num.

% Examples involving a binary tree.
% First using negative bias atom
example 40 (abs a\ abs f\ app a emp)
           (imp (atm n) (imp (imp (atm n) (imp (atm n) (atm n))) (atm n))).
example 41 (abs a\ abs f\ app f (cons (down (app a emp)) (cons (down (app a emp)) emp)))
           (imp (atm n) (imp (imp (atm n) (imp (atm n) (atm n))) (atm n))).
% 16
example 42 (abs a\ abs f\ app f (cons 
  (down (app f (cons (down (app f (cons (down (app f (cons (down (app f (cons (down (app a emp))
  (cons (down (app a emp)) emp)))) (cons (down (app a emp)) emp))))
  (cons (down (app f (cons (down (app a emp)) (cons (down (app a emp)) emp)))) emp))))
  (cons (down (app f (cons (down (app f (cons (down (app a emp)) (cons (down (app a emp)) emp))))
  (cons (down (app f (cons (down (app a emp)) (cons (down (app a emp)) emp)))) emp)))) emp))))
  (cons (down (app f (cons (down (app f (cons (down (app f (cons (down (app a emp)) 
  (cons (down (app a emp)) emp)))) (cons (down (app f (cons (down (app a emp)) (cons (down (app a emp)) emp)))) emp)))) 
  (cons (down (app f (cons (down (app f (cons (down (app a emp)) (cons (down (app a emp)) emp))))
  (cons (down (app f (cons (down (app a emp)) (cons (down (app a emp)) emp)))) emp)))) emp)))) emp)))
           (imp (atm n) (imp (imp (atm n) (imp (atm n) (atm n))) (atm n))).
% 15
example 43 (abs a\ abs f\ app f (cons (down (app f (cons (down (app f (cons (down (app f 
  (cons (down (app f (cons (down (app a emp)) (cons (down (app a emp)) emp))))
  (cons (down (app a emp)) emp)))) (cons (down (app f (cons (down (app a emp)) 
  (cons (down (app a emp)) emp)))) emp)))) (cons (down (app f (cons (down (app f 
  (cons (down (app a emp)) (cons (down (app a emp)) emp)))) (cons (down (app f
  (cons (down (app a emp)) (cons (down (app a emp)) emp)))) emp)))) emp)))) 
  (cons (down (app f (cons (down (app f (cons (down (app f (cons (down (app a emp)) 
  (cons (down (app a emp)) emp)))) (cons (down (app f (cons (down (app a emp))
  (cons (down (app a emp)) emp)))) emp)))) (cons (down (app f (cons (down (app f 
  (cons (down (app a emp)) (cons (down (app a emp)) emp)))) (cons (down (app a emp)) emp)))) emp)))) emp)))
           (imp (atm n) (imp (imp (atm n) (imp (atm n) (atm n))) (atm n))).
example 44 Tree (imp (atm n) (imp (imp (atm n) (imp (atm n) (atm n))) (atm n))) :- 
  neg_tree (s (s (s (s (s (s (s (s z)))))))) Tree.
% Second using positive bias atom
example 50 (abs a\ abs f\ up a)
           (imp (atm p) (imp (imp (atm p) (imp (atm p) (atm p))) (atm p))).
example 51 (abs a\ abs f\ app f (cons a (cons a (kappa x\ up x))))
           (imp (atm p) (imp (imp (atm p) (imp (atm p) (atm p))) (atm p))).
example 52 (abs a\ abs f\ app f (cons a (cons a (kappa x\ up a))))
           (imp (atm p) (imp (imp (atm p) (imp (atm p) (atm p))) (atm p))).
example 53 (abs a\ abs f\ app f (cons a (cons a (kappa x\ app f (cons x (cons x (kappa y\ up y)))))))
           (imp (atm p) (imp (imp (atm p) (imp (atm p) (atm p))) (atm p))).
example 54 Tree (imp (atm p) (imp (imp (atm p) (imp (atm p) (atm p))) (atm p))) :-
  pos_tree (s (s (s (s (s (s (s (s z)))))))) Tree.


%% Develop hybrid term reps for formulas with natural number terms

pos_atom nat.
neg_atom o.

hyp forall   (imp (imp (atm nat) (atm o)) (atm o)).
hyp implies  (imp (atm o) (imp (atm o) (atm o))).
hyp or       (imp (atm o) (imp (atm o) (atm o))).
hyp equal    (imp (atm nat) (imp (atm nat) (atm o))).
hyp zero     (atm nat).
hyp succ     (imp (atm nat) (atm nat)).
hyp ++       (imp (atm nat) (imp (atm nat) (atm nat))).
hyp **       (imp (atm nat) (imp (atm nat) (atm nat))).

% (forall x, x = x)
example 60 (app forall (cons (down (abs x\ app equal (cons x (cons x emp)))) emp)) (atm o).
% (0 + 0)
example 61 (app ++ (cons zero (cons zero (kappa x\ up x))))  (atm nat).
% name x = (0 + 0) in (x = x)
example 62 (app ++ (cons zero (cons zero (kappa x\ app equal (cons x (cons x emp))) ))) (atm o).
% name x = (0 + 0) in name y = (x + x) in (x = y)
example 63 (app ++ (cons zero (cons zero (kappa x\ 
           (app ++ (cons x    (cons x    (kappa y\ app equal (cons x (cons y emp)))))))))) (atm o).
% forall x, if x*x + 6 = 5*x then (x = 2 or x = 3)
example 64 
(app forall  (cons (down (abs x\
(app implies (cons (down (app equal (cons x (cons x emp))))
                   (cons (down (app equal (cons x (cons x emp)))) emp))))) emp))
(atm o).

% forall x, name y = (x * x) in (if (x*x = y) then (x = x))
example 65
(app succ (cons zero  (kappa one\
(app succ (cons one   (kappa two\
(app succ (cons two   (kappa three\
(app succ (cons three (kappa four\
(app succ (cons four  (kappa five\
(app succ (cons five  (kappa six\
(app forall (cons (down (abs x\
  (app ** (cons x (cons x (kappa y\ 
    (app implies (cons (down (app equal (cons y (cons x emp))))
                       (cons (down (app equal (cons x (cons x emp))))
                             emp)))
)))))) emp))))))))))))))))))))
(atm o).

% forall x, if x*x + 6 = 5*x then (x = 2 or x = 3)
% name one   = (succ zero ) in name two   = (succ one  ) in 
% name three = (succ two  ) in name four  = (succ three) in 
% name five  = (succ four ) in name six   = (succ five ) in
% forall x, name y = x * x in name w = 5 * x in name u = 6 + y in
%   if (u = w) then ((x = 2) \/ (x = 3))

example 66
(app succ (cons zero  (kappa one\
(app succ (cons one   (kappa two\
(app succ (cons two   (kappa three\
(app succ (cons three (kappa four\
(app succ (cons four  (kappa five\
(app succ (cons five  (kappa six\
(app forall (cons (down (abs x\
  (app ** (cons x    (cons   x (kappa y\ 
  (app ** (cons five (cons   x (kappa w\ 
  (app ++ (cons six  (cons   y (kappa u\
  (app implies (cons (down (app equal (cons u (cons w emp))))
               (cons (down (app or    
                                (cons (down (app equal (cons x (cons two   emp))))
                                (cons (down (app equal (cons x (cons three emp)))) 
                                 emp))))
                emp)))
)))))))))))))) emp))
))))))))))))))))))
(atm o).
