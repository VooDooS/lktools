open Lexing
open Lktools
       
let ex1 = Ast.Abs ("f", fun v -> Ast.App(Ast.Var(v), Ast.Var(v)))
                

                  
let parse lexbuf = Parser.inlist Lexer.read lexbuf

let parse_string s =
  parse (from_string s)

let parse_file filename =
  let inc = open_in filename in
  parse (from_channel inc)
  
let list = parse_file "exemples.txt" (* (parse_string "abs x. (x empty) 
abs x. x empty")*)
let () = Ast.printl list
let () = printl (List.map (fromLtoLK) list);

                    
