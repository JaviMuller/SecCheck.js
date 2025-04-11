open Yojson.Safe.Util

type t =
  | Predicate of (string * string list)
  | HasName of (string * string)
  | HasValue of (string * Value.t)
  | Not of t
  | And of t list
  | Or of t list

let rec flatten (f : t) : t =
  match f with
  | Not f' -> Not (flatten f')
  | And lst ->
      let flattened_list = List.fold_left (fun acc subformula ->
                            match flatten subformula with
                            | And sublist -> acc @ sublist
                            | other -> acc @ [other]) [] lst in
      And flattened_list
  | Or lst ->
      let flattened_list = List.fold_left (fun acc subformula ->
                            match flatten subformula with
                            | Or sublist -> acc @ sublist
                            | other -> acc @ [other]) [] lst in
      Or flattened_list
  | _ -> f

let rec to_string_aux (f : t) : string = 
  match f with
  | Predicate (name, args) -> name ^ "(" ^ (String.concat ", " args) ^ ")"
  | HasName (var, name) -> "Name(" ^ var ^ ")" ^ " == " ^ name
  | HasValue (var, v) -> "Value(" ^ var ^ ")" ^ " == " ^ (Value.to_string v)
  | Not f' -> "\u{00AC} " ^ (to_string_aux f')
  | And fs -> "(" ^ String.concat " \u{2227} " (List.map to_string_aux fs) ^ ")"
  | Or fs -> "(" ^ String.concat " \u{2228} " (List.map to_string_aux fs) ^ ")" 

let to_string (f : t) : string =
  to_string_aux @@ flatten f

let rec to_yojson_aux (f : t) : Yojson.Safe.t =
  let formula = match f with
  | Predicate (name, args) ->
    `Assoc [ ("type", `String "Predicate");
             ("name", `String name);
             ("args", `List (List.map (fun x -> `String x) args)) ]
  | HasName (var, name) ->
    `Assoc [ ("type", `String "HasName");
             ("variable", `String var);
             ("name", `String name) ]
  | HasValue (var, v) ->
    `Assoc [ ("type", `String "HasValue");
             ("variable", `String var);
             ("value", `String (Value.to_string v)) ]
  | Not f' ->
    `Assoc [ ("type", `String "Not");
             ("formula", to_yojson_aux f') ]
  | And fs ->
    `Assoc [ ("type", `String "And");
             ("formulas", `List (List.map to_yojson_aux fs)) ]
  | Or fs ->
    `Assoc [ ("type", `String "Or");
             ("formulas", `List (List.map to_yojson_aux fs)) ] in
  `Assoc [ ("type", `String "Formula");
           ("formula", formula) ]

let to_yojson (f : t) : Yojson.Safe.t =
  to_yojson_aux @@ flatten f

let rec of_yojson (json : Yojson.Safe.t) : (t, string) result =
  try
    let json_to_string = Yojson.Safe.Util.to_string in
    match json with
    | `Assoc _ ->
      let top_type = member "type" json |> json_to_string in
      if top_type <> "Formula" then
        Error ("Expected type Formula but got: " ^ top_type)
      else
        let formula_json = member "formula" json in
        begin
          match formula_json with
          | `Assoc _ ->
            let formula_type = member "type" formula_json |> json_to_string in
            let sequence_results (results: (t, string) result list) : (t list, string) result =
              let rec aux acc = function
                | [] -> Ok (List.rev acc)
                | (Ok x) :: xs -> aux (x :: acc) xs
                | (Error m) :: _ -> Error m in
              aux [] results in
            (
              match formula_type with
              | "Predicate" ->
                let name = member "name" formula_json |> json_to_string in
                let args = member "args" formula_json |> to_list |> List.map json_to_string in
                Ok (Predicate (name, args))
              | "HasName" ->
                let var = member "variable" formula_json |> json_to_string in
                let name = member "name" formula_json |> json_to_string in
                Ok (HasName (var, name))
              | "HasValue" ->
                let var = member "variable" formula_json |> json_to_string in
                let value = member "value" formula_json |> Value.of_yojson in
                (match value with
                 | Ok v -> Ok (HasValue (var, v))
                 | Error m -> Error m)
              | "Not" ->
                let f = member "formula" formula_json |> of_yojson in
                (match f with
                 | Ok f -> Ok (Not f)
                 | Error m -> Error m)
              | "And" ->
                let fs = member "formulas" formula_json
                         |> to_list
                         |> List.map of_yojson
                         |> sequence_results in
                (match fs with
                 | Ok fs -> Ok (And fs)
                 | Error m -> Error m)
              | "Or" ->
                let fs = member "formulas" formula_json
                         |> to_list
                         |> List.map of_yojson
                         |> sequence_results in
                (match fs with
                 | Ok fs -> Ok (Or fs)
                 | Error m -> Error m)
              | s ->
                Error ("Unknown formula type: " ^ s)
            )
          | _ ->
            Error "Expected the \"formula\" field to be an object"
        end
    | _ ->
      Error "Expected a JSON object at the top level"
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)