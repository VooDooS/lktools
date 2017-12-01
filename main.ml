open Lexing

       
let ex1 = Lk.Abs (fun v -> Lk.App(v, Lk.Empty))
                

                  
let parse lexbuf = Parser.inlist Lexer.read lexbuf

let parse_string s =
  parse (from_string s)

let parse_file filename =
  let inc = open_in filename in
  parse (from_channel inc)
  
let () = Lktools.printl (parse_file "exemples.txt") (* (parse_string "abs x. (x empty) 
abs x. x empty")*)
