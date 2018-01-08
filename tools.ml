let debug = false


let print_debug s =
  if debug then begin
      print_string ("Dbg: " ^ s);
      print_newline ()
    end;

