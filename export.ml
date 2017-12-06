open Lk

let lkTolpl lkl =

  let rec tolpl tm =
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
        App(v, cont) -> "(app " ^ (aux_value v) ^ " " ^ (aux_cont cont) ^ ")"
      | Abs(hint, abs) -> let id = fresh hint in
                          "abs " ^ id ^ "\\ " ^ aux (abs (Ident id))
      | Up(v) -> "(up " ^ (aux_value v) ^ ")"
    and aux_cont = function
      Empty -> "ɛ"
      | Kappa(hint, kap) -> let id = fresh hint in
                            " (kappa " ^ id ^ "\\ "
                            ^ (aux (kap (Ident id))) ^ ")"
      | Cons(v, c) -> "(cons " ^ (aux_value v) ^ (aux_cont c) ^ ")"
    and aux_value = function
        Down tm -> "(down " ^ (aux tm)  ^ ")"
      | Ident(s) -> s
    in aux tm
  in


  let make_example =
    let counter = ref 100 in
    fun tm ->
    counter := !counter + 1;
    "example " ^ (string_of_int !counter) ^ " (" ^ (tolpl tm) ^ ") TYPE.\n"
    in

  let lkterms = open_out "export/gen.mod" in
  output_string lkterms "module gen.\naccumulate termrep.\n\n";
  List.iter (fun x -> output_string lkterms (make_example x)) lkl
