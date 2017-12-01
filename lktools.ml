open Lk

(* This function, used by the parser bind an indent in a term *)
let build_abs term ident =
  let rec aux t =
    match t with
    | App(v, c) ->
       fun x -> App ((aux_v v) x, (aux_c c) x)
    | Abs(abs) ->
       fun x -> Abs (fun y -> aux (abs y) x)
    | Up(v) ->
       fun x -> Up ((aux_v v) x)
  and aux_v = function
    | Ident(s) ->
       (* print_string ("s: " ^ s ^ " ident: " ^ ident ^ "\n"); *)
       if s = ident then
         fun x -> x
       else fun x -> Ident(s)
    | Down(t) ->
       fun x -> Down ((aux t) x)
  and aux_c = function
    | Empty -> fun x -> Empty
    | Kappa(abs) ->
       fun x -> Kappa (fun y -> aux (abs y) x)
    | Cons(v, c) ->
       fun x -> Cons((aux_v v) x, (aux_c c) x)
    
  in
  aux term
      
       
let print tm =
  let fresh = 
    let i = ref 0 in
    fun () -> i := !i +1; "x" ^ string_of_int !i
  in
  let rec aux = function
      App(v, cont) -> "(" ^ (aux_value v) ^ " " ^ (aux_cont cont) ^ ")"
    | Abs(abs) -> let id = fresh () in
                  "λ" ^ id ^ "." ^ aux (abs (Ident id))
    | Up(v) -> "↑" ^ aux_value v
  and aux_cont = function
      Empty -> "ɛ"
    | Kappa(kap) -> let id = fresh () in
                    "κ" ^ id ^ "." ^ aux (kap (Ident id))
    | Cons(v, c) -> (aux_value v) ^ "::" ^ aux_cont c
  and aux_value = function
      Down tm -> "↓" ^ aux tm
    | Ident(s) -> s
  in
  print_string (aux tm)

let printl = List.iter (fun tm -> print tm; print_newline ())
