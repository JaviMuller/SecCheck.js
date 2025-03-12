type t =
  | Eq
  | NEq
  | Lt
  | LEq
  | Gt
  | GEq
  
let to_string (op : t) : string =
  match op with
  | Eq -> "=="
  | NEq -> "!="
  | Lt -> "<"
  | LEq -> "<="
  | Gt -> ">"
  | GEq -> ">="
