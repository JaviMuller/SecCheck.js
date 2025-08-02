open Yojson.Safe.Util

type t = 
  | BaseTaint  of Qvar.t
  | Sink       of Qvar.t
  | Sanitizer  of Qvar.t
  | HasValue   of Qvar.t * Value.t
  | HasFunName of Qvar.t * string
  | Depends    of Qvar.t * Qvar.t
  | Tainted    of Qvar.t
  | SameValue  of Qvar.t * Qvar.t

let to_string (p : t) : string =
  match p with
  | BaseTaint v -> "base_taint(" ^ Qvar.to_string v ^ ")"
  | Sink v -> "sink(" ^ Qvar.to_string v ^ ")"
  | Sanitizer v -> "sanitizer(" ^ Qvar.to_string v ^ ")"
  | HasValue (v1, v2) -> "has_value(" ^ Qvar.to_string v1 ^ ", " ^ Value.to_string v2 ^ ")"
  | HasFunName (f, n) -> "has_fname(" ^ Qvar.to_string f ^ ", " ^ n ^ ")"
  | Depends (v1, v2) -> "depends(" ^ Qvar.to_string v1 ^ "," ^ Qvar.to_string v2 ^ ")"
  | Tainted v -> "tainted(" ^ Qvar.to_string v ^")"
  | SameValue (v1, v2) -> "same_value(" ^ Qvar.to_string v1 ^ ", " ^ Qvar.to_string v2 ^ ")"

let to_yojson (p : t) : Yojson.Safe.t =
  match p with
  | BaseTaint v ->
    `Assoc [ ("type", `String "BaseTaint");
             ("var", `String (Qvar.to_string v)) ]
  | Sink v ->
    `Assoc [ ("type", `String "Sink");
             ("var", `String (Qvar.to_string v)) ]
  | Sanitizer v ->
    `Assoc [ ("type", `String "Sanitizer");
             ("var", `String (Qvar.to_string v)) ]
  | HasValue (v1, v2) ->
    `Assoc [ ("type", `String "HasValue");
             ("var", `String (Qvar.to_string v1));
             ("value", Value.to_yojson v2) ]
  | HasFunName (f, n) ->
    `Assoc [ ("type", `String "HasFunName");
             ("var", `String (Qvar.to_string f));
             ("name", `String n) ]
  | Depends (v1, v2) ->
    `Assoc [ ("type", `String "Depends");
             ("var1", `String (Qvar.to_string v1));
             ("var2", `String (Qvar.to_string v2)) ]
  | Tainted v ->
    `Assoc [ ("type", `String "Tainted");
             ("var", `String (Qvar.to_string v)) ]
  | SameValue (v1, v2) ->
    `Assoc [ ("type", `String "SameValue");
             ("var1", `String (Qvar.to_string v1));
             ("var2", `String (Qvar.to_string v2)) ]

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  let json_to_string = Yojson.Safe.Util.to_string in
  try
    match json with
    | `Assoc _ ->
      let t = member "type" json |> json_to_string in
      (match t with
       | "BaseTaint" ->
         let var = member "var" json |> json_to_string in
         Ok (BaseTaint (Qvar.Var var))
       | "Sink" ->
         let var = member "var" json |> json_to_string in
         Ok (Sink (Qvar.Var var))
       | "Sanitizer" ->
         let var = member "var" json |> json_to_string in
         Ok (Sanitizer (Qvar.Var var))
       | "HasValue" ->
         let var = member "var" json |> json_to_string in
         let value = member "value" json |> Value.of_yojson in
         (match value with
         | Ok v -> Ok (HasValue (Qvar.Var var, v))
         | Error m -> Error m)
       | "HasFunName" ->
         let var = member "var" json |> json_to_string in
         let name = member "name" json |> json_to_string in
         Ok (HasFunName (Qvar.Var var, name))
       | "Depends" ->
         let var1 = member "var1" json |> json_to_string in
         let var2 = member "var2" json |> json_to_string in
         Ok (Depends (Qvar.Var var1, Qvar.Var var2))
       | "Tainted" ->
         let var = member "var" json |> json_to_string in
         Ok (Tainted (Qvar.Var var))
       | "SameValue" ->
         let var1 = member "var1" json |> json_to_string in
         let var2 = member "var2" json |> json_to_string in
         Ok (SameValue (Qvar.Var var1, Qvar.Var var2))
        | _ -> Error "Unexpected predicate")
    | _ -> Error "Expected a JSON object at the top level"
  with
    | Type_error (msg, _) ->
      Error ("Type error during parsing: " ^ msg)
    | e ->
      Error ("Unexpected error: " ^ Printexc.to_string e)