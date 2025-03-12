type t =
  | Bool of bool
  | Int of int
  | Float of float
  | String of string

let to_string(v : t) : string =
  match v with
  | Bool x -> string_of_bool x
  | Int x -> string_of_int x
  | Float x -> string_of_float x
  | String x -> x
