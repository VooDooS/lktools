open Lexing

       
let ex1 = Lk.Abs (fun v -> Lk.App(v, Lk.Empty))
                

                  
let parse lexbuf = Parser.inlist Lexer.read lexbuf

let parse_string s =
  parse (from_string s)

let parse_file filename =
  let inc = open_in filename in
  parse (from_channel inc)

let lklist = parse_file "exemples.txt"

let t1 = List.hd lklist
let t2 = List.hd (List.tl lklist)

let () = Lktools.printl lklist; Printf.printf "%B" (Lktools.equal t1 t2)
