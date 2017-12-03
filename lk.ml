type tm =
  | App of value  * cont
  | Abs of (value -> tm)
  | Up of value
and cont =
  | Empty
  | Kappa of (value -> tm)
  | Cons of value * cont
and value =

  | Ident of string
  | Down of tm
