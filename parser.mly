%{

  open Ast

%}

%token ABS
%token DOT
%token SEMI
%token <string> IDENT
%token LEFT_BRACE
%token RIGHT_BRACE
%token EOF

%type <Ast.tm list> inlist
%start inlist
%%

inlist:
  | EOF				{ [] }
  | t = term; EOF		{ [t] }
  | t = term; SEMI; l = inlist	{ t::l }
  ;

term:
  | ABS; i=IDENT; DOT; t=term   { Abs (build_abs t i) }
  |  LEFT_BRACE; t1 = term; t2 = term; RIGHT_BRACE	{ App (t1, t2) }
  | i = IDENT { Var(i) }
  ;