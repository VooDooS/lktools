open Lexing
open Lktools
       
let ex1 = Ast.Abs ("f", fun v -> Ast.App(Ast.Var(v), Ast.Var(v)))
                

                  
let parse lexbuf = Parser.inlist Lexer.read lexbuf

let parse_string s =
  parse (from_string s)

let parse_file filename =
  let inc = open_in filename in
  parse (from_channel inc)
        
let list = parse_file "examples.txt" (* (parse_string "abs x. (x empty) 
abs x. x empty")*)
let () = Ast.printl list
(*let () = print (fromLtoLK (List.hd list))*)
let lklist = List.map (fromLtoLK) list
let lksharelist = List.map (share) lklist
let () = Lk.printl lklist
let () = Lk.printl lksharelist

let () = Export.lkTolpl lklist


(*
let t = LKHash.create 256
let _ = LKHash.add t (Lk.App (Lk.Ident "f", Lk.Empty))

let s = try LKHash.find t (Lk.App (Lk.Ident "f", Lk.Empty)); true with
          Not_found -> false*)

let () = if s then print_string "ok" else print_string "pasok"
          
