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


let rec getLast cons = match cons with
    Cons(_, c) -> getLast c
  | _ -> cons

let rec replaceKappa  cons kappa =
  match cons with
  | Kappa(_, _) -> kappa
  | Cons(v, c) -> Cons(v, replaceKappa c kappa)
  | _ -> failwith "No Kappa found, failed to replace"

                  
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
      Down tm -> "↓" ^ aux tm
    | Ident(s) -> s
  in
  print_string (aux tm)

let printl = List.iter (fun tm -> print tm; print_newline ())


let fromLtoLK lt =
  let varTab = Hashtbl.create 256 in
  let rec fresh = 
    let i = ref 0 in
    fun hint ->
    match hint with
      None ->(let n =  i := !i +1; "x" ^ string_of_int !i in
      try let _ = Hashtbl.find varTab n in fresh hint
      with Not_found -> Hashtbl.add varTab n n; n)
    | Some h -> (
    try
      let _ = Hashtbl.find varTab h in fresh None
    with Not_found -> Hashtbl.add varTab h h; h)
  in

  let free var =
    Hashtbl.remove varTab var
  in

  
  (* The share function does Hashconsing *)
  (* let share =
    let table = Hashtbl.create 256 in
    fun lkt ->
    try Hashtbl.find table lkt with
      Not_found -> Hashtbl.add table lkt lkt; lkt
  in *)

  
  let rec apply_under_kappa f a =
    match a with
    | App(f2, c) ->
       let rec aux_replace c =
         match c with
         | Kappa(hint, abs) ->
            let fi = fresh (Some hint) in
            (match (abs (Ident(fi))) with
             | Up(Ident(i)) -> 
                Kappa(hint, fun x -> App(f, Cons(x, Kappa(fi, fun x -> Up x))))
             | App(_, _) as a -> Kappa(fi, build_abs (apply_under_kappa f a) fi)
             | _ -> failwith "Not an app nor an up")
      | Cons(v, c) -> Cons(v, aux_replace c)
      | _ -> failwith "found empty while applying under kappa !"
       in
       App(f2, aux_replace c)
    | _ -> failwith "App expected in apply_under_kappa"
  in
  
  let rec add_before_kappa x c =
    match c with
    | Kappa(_,_) as k -> Cons(x, k)
    | Cons(v, c) -> Cons(v,  add_before_kappa x c)
    | _ -> failwith "found empty while adding before kappa !"
  in
  

  
  let rec aux lt  =
    match lt with
    | Ast.App(lt1, lt2) ->
       aux_app lt1 lt2
    | Ast.Abs(hint, abs) ->
       Abs(hint, build_abs (aux (free hint;abs hint)) hint)
    |tm -> Up(aux_v tm)
  and aux_v lt =
    match lt with
    | Ast.Var(i) -> Ident(i)
    | tm -> Down(aux tm)
  and aux_app v lt =
    (** 
        Three important cases :
        
     1. app (app f x) x
        --> f x::x::Kup
        (same with abstractions)

     2. app f (app f x)
        --> f x::kx1.(f x1::kup))
       
     3. app (app f x) (app f x) --> f x::Kx1. up x1
                |
                v                         v
           f x::kx2.up x2            >  merge 
                                          v
                             f x::kx1. (f x::x1::kx2.x2)
     *)
    match v, lt  with
    | Ast.Var(f), (Ast.Var(_) as b)
      | Ast.Var(f), (Ast.Abs(_, _) as b)
      -> App(Ident(f), Cons(aux_v b,
                            Kappa(fresh None, (fun y -> Up y))))
   
            
    | Ast.Var(f1), Ast.App(f2, c)
      -> let inc =  aux_app f2 c in
         apply_under_kappa (Ident f1) inc
                           
    | Ast.App(f, c), (Ast.Var(_) as b)
      | Ast.App(f, c) , (Ast.Abs(_, _) as b)
      -> let a =  aux_app f c in
         (match a with
            App(v2, c2) -> App( v2, add_before_kappa (aux_v b) c2)
          | _ -> failwith "This is not an app")
           
    | Ast.App(f1, c1), Ast.App(f2, c2)
      -> let a1 =  aux_app f1 c1
         and a2 = aux_app f2 c2 in
         (match a1, a2 with
            App(f1', c1'), App(f2', c2') ->
             (
               let k1 = getLast c1' in
               (match k1 with
                  Kappa (hint, abs) ->
                   let x1 = fresh (Some hint) in
                   let x2 = fresh None in
                   let  a = abs (Ident x1) in
                   (match a with
                      App(f, kx1) -> 
                       let lastKappa =
                         Kappa(x2,
                               build_abs (App(f, add_before_kappa (Ident x2) kx1))
                                         x2
                              )
                       in
                       let firstKappa =
                         Kappa(x1,
                               (build_abs (App(
                                               f2',
                                               replaceKappa c2' lastKappa
                                             )
                                          )
                                          x1
                               )
                              )
                       in

                       App(f1', replaceKappa c2' firstKappa)
                    | _ -> failwith "Not an app"
                   )
                | _ -> failwith "Looked for Kappa, found Emp"
               )
             )
          | _ -> failwith "This is not an app")
    | Ast.Abs(_), _ -> failwith "Your term is not normal"
                       
                       
  in
  aux lt 
      
      
