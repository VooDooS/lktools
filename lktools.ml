open Lk

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

let equal tm1 tm2 =
  let fresh = 
    let i = ref 0 in
    fun () -> i := !i +1; "x" ^ string_of_int !i
  in
  let rec aux tm1 tm2 =
    match tm1, tm2 with
      App(v1, c1), App(v2, c2) -> (aux_v v1 v2) && (aux_c c1 c2)
    | Abs(_,abs1), Abs(_, abs2) ->
       let x = fresh () in aux (abs1 (Ident x)) (abs2 (Ident x))
    | Up(v1), Up(v2) -> aux_v v1 v2
    | _ -> false
  and aux_v v1 v2 =
    match v1, v2 with
    | Ident(s1), Ident(s2) ->
       if s1 = s2 then true else false
    | Down(t1), Down(t2) -> aux t1 t2
    | _ -> false
  and aux_c c1 c2 =
    match c1, c2 with
    | Empty, Empty -> true
    | Kappa(_, abs1), Kappa(_, abs2) ->
       let x = fresh () in aux (abs1 (Ident x)) (abs2 (Ident x))
    | Cons(v1, c1), Cons(v2, c2) -> (aux_v v1 v2) && (aux_c c1 c2)
    | _ -> false
  in
  aux tm1 tm2
      
       
let print tm =
  let fresh = 
    let i = ref 0 in
    fun hint -> hint (* TODO *)
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
      Down tm -> "↓" ^ aux tm
    | Ident(s) -> s
  in
  print_string (aux tm)

let printl = List.iter (fun tm -> print tm; print_newline ())


let fromLtoLK lt =
  let fresh = 
    let i = ref 0 in
    fun () -> i := !i +1; "x" ^ string_of_int !i
  in
  (* The share function does Hashconsing *)
  let share =
    let table = Hashtbl.create 256 in
    fun lkt ->
    try Hashtbl.find table lkt with
      Not_found -> Hashtbl.add table lkt lkt; lkt
  in 
  let rec aux lt  =
    match lt with
    | Ast.App(lt1, lt2) ->
       let out, cons = aux_c lt1 lt2 in
       App(out, cons)
    | Ast.Abs(hint, abs) ->
       Abs(hint, build_abs (aux (abs hint)) hint)
    |tm -> Up(aux_v tm)
  and aux_v lt =
    match lt with
    | Ast.Var(i) -> Ident(i)
    | tm -> Down(aux tm)
  and aux_c v lt =
    match v  with
    | Ast.Var(i) -> Ident(i), Cons(aux_v lt, Kappa(fresh (), fun x -> Up x))
    | Ast.App(lt1, lt2) ->
       let v', lt' = aux_c lt1 lt2 in
       v', Cons(aux_v lt, lt')
    | _ -> assert false
                            
       
  in
  aux lt 
 
