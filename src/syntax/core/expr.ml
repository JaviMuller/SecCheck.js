type conc =
  | Loc of string
  | Val of Value.t

type abs =
  | UVar
  | Var of string
  | Loc of string
  | Val of Value.t


let is_concrete (f : abs) : bool =
  match f with
  | Var _ -> false
  | _ -> true

let to_conc (f : abs) : conc option =
  match f with
  | UVar -> None
  | Var _ -> None
  | Loc l -> Some (Loc l)
  | Val v -> Some (Val v)

let abs_to_string (e : abs) : string =
  match e with
  | UVar -> "UVar()"
  | Var v -> "Var(" ^ v ^ ")"
  | Loc l -> "Loc(" ^ l ^ ")"
  | Val v -> "Val(" ^ Value.to_string v ^ ")"

let conc_to_string (e : conc) : string =
  match e with
  | Loc l -> "Loc(" ^ l ^ ")"
  | Val v -> "Val(" ^ Value.to_string v ^ ")"

let abs_to_yojson (e : abs) : Yojson.Safe.t =
  match e with
  | UVar ->
    `Assoc [ ("type", `String "UnnamedVariable") ]
  | Var x ->
    `Assoc [ ("type", `String "Variable");
             ("value", `String x) ]
  | Loc l ->
    `Assoc [ ("type", `String "Location");
             ("value", `String l) ]
  | Val v ->
    `Assoc [ ("type", `String "Value");
             ("value", Value.to_yojson v) ]

let abs_of_yojson (json : Yojson.Safe.t) : (abs, string) result =
  let open Yojson.Safe.Util in
  try
    match json with
    | `Assoc _ ->
      let json_to_string = Yojson.Safe.Util.to_string in
      let abs_type = member "type" json |> json_to_string in
      (match abs_type with
      | "UnnamedVariable" -> Ok UVar
      | "Variable" ->
        let value = member "value" json |> json_to_string in
        Ok (Var value)
      | "Location" ->
        let value = member "value" json |> json_to_string in
        Ok (Loc value)
      | "Value" ->
        let value = member "value" json |> Value.of_yojson in
        (match value with
        | Ok v -> Ok (Val v)
        | Error m -> Error m)
      | _ -> Error ("Unknown type: " ^ abs_type))
    | _ -> Error "Invalid JSON format"
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)

let conc_to_yojson (e : conc) : Yojson.Safe.t =
  match e with
  | Loc l ->
    `Assoc [ ("type", `String "Location");
             ("value", `String l) ]
  | Val v ->
    `Assoc [ ("type", `String "Value");
             ("value", Value.to_yojson v) ]

let conc_of_yojson (json : Yojson.Safe.t) : (conc, string) result =
  let open Yojson.Safe.Util in
  try
    match json with
    | `Assoc _ ->
      let json_to_string = Yojson.Safe.Util.to_string in
      let conc_type = member "type" json |> json_to_string in
      (match conc_type with
      | "Location" ->
        let value = member "value" json |> json_to_string in
        Ok (Loc value)
      | "Value" ->
        let value = member "value" json |> Value.of_yojson in
        (match value with
        | Ok v -> Ok (Val v)
        | Error e -> Error e)
      | _ -> Error ("Unknown type: " ^ conc_type))
    | _ -> Error "Invalid JSON format"
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)
