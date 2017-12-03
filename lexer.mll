{
open Lexing
open Parser

exception SyntaxError of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "abs",     ABS;
        "app", 	   APP ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
}

let digit = ['0'-'9']
let int = '-'? digit+

let alpha = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' '_'] (alpha | '_' | '\'' | digit)*

let comment = '%'  [^ '\n' '\r']*

rule read =
  parse
  |  ['\n' ' ' '\t' '\r']+	{ read lexbuf }
  | comment  	    		{ read lexbuf  }
  | id				{ id_or_keyword (lexeme lexbuf) }
  | "."	| "\\"			{ DOT }
  | ";"				{ SEMI }
  | "(" 			{ LEFT_BRACE }
  | ")" 			{ RIGHT_BRACE }
  | _  	  			{ failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof      			{ EOF }

