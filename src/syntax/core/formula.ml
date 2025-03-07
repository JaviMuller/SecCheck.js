type t =
  | True
  | False 

let to_string (f : t) : string =
  match f with
  | True -> "true"
  | False -> "false"
