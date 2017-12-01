%{

  open Lk

%}

%token ABS
%token KAPPA
%token UP
%token DOWN
%token EMPTY
%token CONS
%token CONSPREF
%token <string> IDENT
%token DOT
%token LEFT_BRACE
%token RIGHT_BRACE
%token EOF

%type <Lk.tm list> inlist
%start inlist
%%

inlist:
  | EOF				{ [] }
  | t = term; l = inlist	{ t::l }
  ;

term:
  | LEFT_BRACE; t=term; RIGHT_BRACE
				{ t }
  | ABS; i=IDENT; DOT; t=term   { Abs (Lktools.build_abs t i) }
  | v = value; c = cont		{ App (v, c) }
  | UP; v = value 		{ Up (v) }
  ;

value:
  | LEFT_BRACE; v=value; RIGHT_BRACE
				{ v }
  | i = IDENT			{ Ident i }
  | DOWN; t = term 		{ Down t }
  ;

cont:
  | LEFT_BRACE; c=cont; RIGHT_BRACE
				{ c }
  | EMPTY			{ Empty }
  | KAPPA; i=IDENT; DOT; t=term { Kappa (Lktools.build_abs t i) }
  | CONSPREF; v = value;  c = cont
				{ Cons(v, c) }
  | v = value; CONS; c = cont 	{ Cons(v, c) }
  ;