open Lk



let rec getLast cons = match cons with
    Cons(_, c) -> getLast c
  | _ -> cons

let rec replaceKappa  cons kappa =
  match cons with
  | Kappa(_, _) -> kappa
  | Cons(v, c) -> Cons(v, replaceKappa c kappa)
  | _ -> failwith "No Kappa found, failed to replace"

                  
let equal tm1 tm2 =
  Tools.print_debug ("Testing: " ^ (toString tm1) ^ " and " ^ (toString tm2));
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
  match aux tm1 tm2 with
    true -> 
     Tools.print_debug ("Equal !"); true
  | false -> 
     Tools.print_debug ("Not equal !"); false


module LKHash = Hashtbl.Make(struct
  type t = tm
  let equal = (print_string "pouet"; equal)
  let hash = Hashtbl.hash
end)

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
      -> let id = fresh None in
         App(Ident(f), Cons(aux_v b,
                            Kappa(id, (fun y -> Up y))))
   
            
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
    | Ast.Abs(_), _ -> failwith "Term is not normal"
                       
                       
  in
  aux lt 
      
      
let share lk =
  (* TODO Separate freshtools in another module *)
  let varTab = Hashtbl.create 256 in
  let shareAssoc = ref [] in
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
  
  (* The share function does Hashconsing to build 
     λκ-terms with maximum sharing *)
  let share_aux =
    fun lkt hint ->
    Tools.print_debug ("Sharing : " ^ (toString lkt));
    match Myassoc.find !shareAssoc equal lkt with
      None ->
       let id = hint  in
       Tools.print_debug ("Not found");
       shareAssoc := Myassoc.add !shareAssoc equal lkt id;
       id, false
    | Some(id') -> id', true
    (*try Some(LKHash.find table lkt);
    with
    | Not_found ->
       let id = fresh None in
       Tools.print_debug ("Not found");
       LKHash.add table lkt id; None *)
  in

  let rec split = function
    | Empty -> failwith "Truncate should not find Empty"
    | Cons(v, c) -> let c,k = split c in
                    Cons(v, c), k
    | Kappa(h, _ ) as k -> Kappa(h, fun x -> Up x), k
  in

  (* let rec ne *)
  
  let rec aux = function
    | App(v, c) -> let newV = aux_v v in
       let trunk, kapp = split c in
       (match kapp with
        | Kappa(hint, abs) -> (
          let id, sharedtm = share_aux (App(newV, trunk)) hint in
          if sharedtm then (
            Tools.print_debug ("Replacing "^ hint ^" by "^id);
            aux (abs (Ident id))
          (* TODO : this probably wrong as it does not rebind id in the term *)
          ) else App(newV, aux_c c )
        )
        | _ -> failwith "Oups, waiting for Kappa..."
       )
    | Abs(hint, abs) ->
       let newTerm = aux (abs (Ident(hint))) in
       let newAbs = build_abs newTerm hint  in
         Abs(hint, newAbs)
    | Up(v) -> Up(v)
  and aux_v = function
    | Ident(s) -> Ident(s)
    | Down(tm) -> Down(tm)
  and aux_c = function
    | Empty -> Empty
    | Kappa(hint, abs) ->
       let fresh = fresh (Some(hint)) in
       Kappa(hint, build_abs (aux (abs (Ident fresh))) fresh)
    | Cons(v, c) -> Cons(aux_v v, aux_c c)
  in
  Tools.print_debug ("Adding sharing to " ^ (toString lk));
  aux lk
