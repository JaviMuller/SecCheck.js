open Yojson.Safe.Util

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

let to_yojson(v : t) : Yojson.Safe.t =
  let value = match v with
              | Bool b -> `Bool b
              | Int x -> `Int x
              | Float x -> `Float x
              | String s -> `String s in
  `Assoc [ ("type", `String "Value");
           ("value", value)]

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  let json_to_string = Yojson.Safe.Util.to_string in
  try
    match json with
    | `Assoc _ ->
      let top_type = member "type" json |> json_to_string in
      if top_type <> "Value" then
        Error ("Expected type Value but got: " ^ top_type)
      else 
        let value_json = member "value" json in
        begin
          match value_json with
          | `Bool b -> Ok (Bool b)
          | `Int x -> Ok (Int x)
          | `Float x -> Ok (Float x)
          | `String s -> Ok (String s)
          | _ -> Error ("Unexpected value type")
        end
    | _ ->
      Error ("Expected a JSON object at the top level")
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)
