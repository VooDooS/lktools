let rec find alist eq a =
  match alist with
    [] -> None
  | (a', b'):: tl -> if eq a a' then Some(b')
                     else find tl eq a

let add alist eq a b =
  match find alist eq a with
    None -> (a, b)::alist
  | Some(_) -> alist
                 
