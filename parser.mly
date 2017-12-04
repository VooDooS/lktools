%{

  open Ast

%}

%token ABS APP
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
  | t = term; SEMI; l = inlist	{ t::l }
  | t = term; EOF		{ [t] }
  ;

term:
  | LEFT_BRACE; t = term; RIGHT_BRACE { t }
  | ABS; i=IDENT; DOT; t=term   { Abs (i, (build_abs t i)) }
  | APP; t1 = term; t2 = term	{ App (t1, t2) }
  | i = IDENT { Var(i) }
  ;