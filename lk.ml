type tm =
  | App of value  * cont
  | Abs of string * (value -> tm)
  | Up of value
and cont =
  | Empty
  | Kappa of string * (value -> tm)
  | Cons of value * cont
and value =
  | Ident of string
  | Down of tm
              
(* This function, used by the parser bind an indent in a term *)
let build_abs term ident =
  let rec aux t =
    match t with
    | App(v, c) ->
       fun x -> App ((aux_v v) x, (aux_c c) x)
    | Abs(i, abs) ->
       fun x -> Abs (i, fun y -> aux (abs y) x)
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
    | Kappa(i, abs) ->
       fun x -> Kappa (i, fun y -> aux (abs y) x)
    | Cons(v, c) ->
       fun x -> Cons((aux_v v) x, (aux_c c) x)
                    
  in
  aux term

      
(* This function, used by the parser bind an indent in a term *)
let build_abs term ident =
  let rec aux t =
    match t with
    | App(v, c) ->
       fun x -> App ((aux_v v) x, (aux_c c) x)
    | Abs(i, abs) ->
       fun x -> Abs (i, fun y -> aux (abs y) x)
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
    | Kappa(i, abs) ->
       fun x -> Kappa (i, fun y -> aux (abs y) x)
    | Cons(v, c) ->
       fun x -> Cons((aux_v v) x, (aux_c c) x)
    
  in
  aux term

let toString tm =
  let rec fresh = 
    let i = ref 0 in
    let table = Hashtbl.create 256 in
    fun hint ->
    try
      let _ = Hashtbl.find table hint in
      let n =  i := !i +1; "x" ^ string_of_int !i in
      try let _ = Hashtbl.find table n in fresh hint
      with Not_found -> Hashtbl.add table n n; n
    with Not_found -> Hashtbl.add table hint hint; hint
  in
  let rec aux = function
      App(v, cont) -> "(" ^ (aux_value v) ^ " " ^ (aux_cont cont) ^ ")"
    | Abs(hint, abs) -> let id = fresh hint in
                  "λ" ^ id ^ "." ^ aux (abs (Ident id))
    | Up(v) -> "↑" ^ aux_value v
  and aux_cont = function
      Empty -> "ɛ"
    | Kappa(hint, kap) -> let id = fresh hint in
                    "κ" ^ id ^ "." ^ aux (kap (Ident id))
    | Cons(v, c) -> (aux_value v) ^ "::" ^ aux_cont c
  and aux_value = function
      Down tm -> "↓(" ^ aux tm ^ ")"
    | Ident(s) -> s
  in
  aux tm
        
let print tm =
  print_string (toString tm)

let printl = List.iter (fun tm -> print tm; print_newline ())
