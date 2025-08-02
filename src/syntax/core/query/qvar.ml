open Yojson.Safe.Util

type t =
  | UVar
  | Var of string

let to_string (v : t) : string =
  match v with
  | UVar -> "_"
  | Var s -> s

let to_yojson (v : t) : Yojson.Safe.t =
  match v with
  | UVar ->
    `Assoc [ ("type", `String "UnnamedVariable") ]
  | Var s ->
    `Assoc [ ("type", `String "Variable");
             ("value", `String s) ]

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  let json_to_string = Yojson.Safe.Util.to_string in 
  try
    match json with
    | `Assoc _ ->
      let t = member "type" json |> json_to_string in
      (match t with
      | "UnnamedVariable" -> 
        Ok (UVar)
      | "Varible" ->
        Ok (Var (member "value" json |> json_to_string))
      | _ ->
        Error "Unexpected variable type")
    | _ -> Error "Expected a JSON object at the top level"
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)
