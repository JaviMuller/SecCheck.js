type t =
  | Var of string
  | Const of Value.t

let to_string (expr : t) : string =
  match expr with
  | Var x -> x
  | Const x -> Value.to_string x

