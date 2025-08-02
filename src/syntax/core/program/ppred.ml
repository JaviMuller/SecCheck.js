open Yojson.Safe.Util

type t =
  | BaseTaint  of Ploc.t
  | Sink       of Ploc.t 
  | Sanitizer  of Ploc.t 
  | HasValue   of Ploc.t * Value.abs
  | HasFunName of Ploc.t * string
  | Depends    of Ploc.t * Ploc.t 
  | Tainted    of Ploc.t 
  | SameValue  of Ploc.t * Ploc.t

let to_string (p : t) : string =
  match p with
  | BaseTaint l -> "base_taint(" ^ Ploc.to_string l ^ ")"
  | Sink l -> "sink(" ^ Ploc.to_string l ^ ")"
  | Sanitizer l -> "sanitizer(" ^ Ploc.to_string l ^ ")"
  | HasValue (l1, l2) -> "has_value(" ^ Ploc.to_string l1 ^ ", " ^ Value.abs_to_string l2 ^ ")"
  | HasFunName (f, n) -> "has_fname(" ^ Ploc.to_string f ^ ", " ^ n ^ ")"
  | Depends (l1, l2) -> "depends(" ^ Ploc.to_string l1 ^ "," ^ Ploc.to_string l2 ^ ")"
  | Tainted l -> "tainted(" ^ Ploc.to_string l ^")"
  | SameValue (l1, l2) -> "same_value(" ^ Ploc.to_string l1 ^ ", " ^ Ploc.to_string l2 ^ ")"

let to_yojson (p : t) : Yojson.Safe.t =
  match p with
  | BaseTaint l ->
    `Assoc [ ("type", `String "BaseTaint");
             ("var", `String (Ploc.to_string l)) ]
  | Sink l ->
    `Assoc [ ("type", `String "Sink");
             ("var", `String (Ploc.to_string l)) ]
  | Sanitizer l ->
    `Assoc [ ("type", `String "Sanitizer");
             ("var", `String (Ploc.to_string l)) ]
  | HasValue (l1, l2) ->
    `Assoc [ ("type", `String "HasValue");
             ("var", `String (Ploc.to_string l1));
             ("value", Value.abs_to_yojson l2) ]
  | HasFunName (f, n) ->
    `Assoc [ ("type", `String "HasFunName");
             ("var", `String (Ploc.to_string f));
             ("name", `String n) ]
  | Depends (l1, l2) ->
    `Assoc [ ("type", `String "Depends");
             ("var1", `String (Ploc.to_string l1));
             ("var2", `String (Ploc.to_string l2)) ]
  | Tainted l ->
    `Assoc [ ("type", `String "Tainted");
             ("var", `String (Ploc.to_string l)) ]
  | SameValue (l1, l2) ->
    `Assoc [ ("type", `String "SameValue");
             ("var1", `String (Ploc.to_string l1));
             ("var2", `String (Ploc.to_string l2)) ]

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  let json_to_string = Yojson.Safe.Util.to_string in
  try
    match json with
    | `Assoc _ ->
      let t = member "type" json |> json_to_string in
      (match t with
       | "BaseTaint" ->
         let loc = member "var" json |> json_to_string in
         Ok (BaseTaint (Ploc.Loc loc))
       | "Sink" ->
         let loc = member "var" json |> json_to_string in
         Ok (Sink (Ploc.Loc loc))
       | "Sanitizer" ->
         let loc = member "var" json |> json_to_string in
         Ok (Sanitizer (Ploc.Loc loc))
       | "HasValue" ->
         let loc = member "var" json |> json_to_string in
         let value = member "value" json |> Value.abs_of_yojson in
         (match value with
         | Ok v -> Ok (HasValue (Ploc.Loc loc, v))
         | Error m -> Error m)
       | "HasFunName" ->
         let loc = member "var" json |> json_to_string in
         let name = member "name" json |> json_to_string in
         Ok (HasFunName (Ploc.Loc loc, name))
       | "Depends" ->
         let loc1 = member "var1" json |> json_to_string in
         let loc2 = member "var2" json |> json_to_string in
         Ok (Depends (Ploc.Loc loc1, Ploc.Loc loc2))
       | "Tainted" ->
         let loc = member "var" json |> json_to_string in
         Ok (Tainted (Ploc.Loc loc))
       | "SameValue" ->
         let loc1 = member "var1" json |> json_to_string in
         let loc2 = member "var2" json |> json_to_string in
         Ok (SameValue (Ploc.Loc loc1, Ploc.Loc loc2))
        | _ -> Error "Unexpected predicate")
    | _ -> Error "Expected a JSON object at the top level"
  with
    | Type_error (msg, _) ->
      Error ("Type error during parsing: " ^ msg)
    | e ->
      Error ("Unexpected error: " ^ Printexc.to_string e)