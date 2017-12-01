{
open Lexing
open Parser

exception SyntaxError of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "abs",     ABS;
      	"cons",    CONSPREF;
        "kappa",   KAPPA;
	"empty",   EMPTY;
	"emp",     EMPTY;
        "up",      UP;
	"down",	   DOWN;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
}

let digit = ['0'-'9']
let int = '-'? digit+

let alpha = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' '_'] (alpha | '_' | '\'' | digit)*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  |  ['\n' ' ' '\t' '\r' ';']+	{ read lexbuf }
  |  "app"	    	 	{ read lexbuf }
  | newline  	    		{ Lexing.new_line lexbuf; read lexbuf }
  | id				{ id_or_keyword (lexeme lexbuf) }
  | "."	| "\\"			{ DOT }
  | "::"			{ CONS }
  | "↓"				{ DOWN }
  | "(" 			{ LEFT_BRACE }
  | ")" 			{ RIGHT_BRACE }
  | _  	  			{ failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof      			{ EOF }

