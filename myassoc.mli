val find : ('a * 'b) list -> ('a -> 'a -> bool) -> 'a -> 'b option
val add : ('a * 'b) list ->  ('a -> 'a -> bool) -> 'a -> 'b -> ('a * 'b) list
