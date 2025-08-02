open Yojson.Safe.Util

type t =
  | Bool   of bool
  | Int    of int
  | Float  of float
  | String of string

let to_string(v : t) : string =
  match v with
  | Bool x -> string_of_bool x
  | Int x -> string_of_int x
  | Float x -> string_of_float x
  | String x -> "\"" ^ x ^ "\""

let to_yojson(v : t) : Yojson.Safe.t =
  match v with
  | Bool b -> `Bool b
  | Int x -> `Int x
  | Float x -> `Float x
  | String s -> `String s

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  try
    match json with
    | `Bool b -> Ok (Bool b)
    | `Int x -> Ok (Int x)
    | `Float x -> Ok (Float x)
    | `String s -> Ok (String s)
    | _ -> Error ("Unexpected value type")
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)

type abs = 
  | Val of t
  | Any
  | Many

let abs_to_string (v : abs) : string =
  match v with
  | Val v -> to_string v
  | Any -> "_"
  | Many -> "*"

let abs_to_yojson (v : abs) : Yojson.Safe.t =
  match v with
  | Val v -> to_yojson v
  | Any -> `String "_"
  | Many -> `String "*"

let abs_of_yojson (json : Yojson.Safe.t) : (abs, string) result =
  try
    match json with
    | `String "_" -> Ok Any
    | `String "*" -> Ok Many
    | _ ->
      (match of_yojson json with
      | Ok v -> Ok (Val v)
      | Error e -> Error e)
  with
    | Type_error (msg, _) ->
      Error ("Type error during parsing: " ^ msg)
    | e ->
      Error ("Unexpected error: " ^ Printexc.to_string e)
