type t =
  | UnaryPred of (string * string)

let to_string (p : t) : string =
  match p with
  | UnaryPred (name, arg) -> "." ^ name ^ "(" ^ arg ^ ")"
