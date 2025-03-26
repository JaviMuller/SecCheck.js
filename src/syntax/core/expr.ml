type t =
  | Var of string
  | UnnamedVar
  | Const of Value.t

let to_string (expr : t) : string =
  match expr with
  | Var x -> x
  | UnnamedVar -> "_"
  | Const x -> Value.to_string x

