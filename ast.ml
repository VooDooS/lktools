type tm =
  | App of tm  * tm
  | Abs of (string -> tm)
  | Var of string


(* This function, used by the parser bind an indent in a term *)
let build_abs term ident =
  let rec aux t =
    match t with
    | App(t1, t2) ->
       fun x -> App ((aux t1) x, (aux t2) x)
    | Abs(abs) ->
       fun x -> Abs (fun y -> aux (abs y) x)
    | Var(s) ->
       (* print_string ("s: " ^ s ^ " ident: " ^ ident ^ "\n"); *)
       if s = ident then
         fun x -> Var(x)
       else fun x -> Var(s)
  in
  aux term
      

let print tm =
  let fresh = 
    let i = ref 0 in
    fun () -> i := !i +1; "x" ^ string_of_int !i
  in
  let rec aux = function
      App(t1, t2) -> "(" ^ (aux t1) ^ " " ^ (aux t2) ^ ")"
    | Abs(abs) -> let id = fresh () in
                  "λ" ^ id ^ "." ^ aux (abs (id))
    | Var(s) -> s
  in
  print_string (aux tm)

let printl = List.iter (fun tm -> print tm; print_newline ())
