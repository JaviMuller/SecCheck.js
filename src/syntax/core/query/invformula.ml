open Yojson.Safe.Util

type t =
  | True
  | False
  | Pred  of Qpred.t
  | Not   of t
  | And   of t list
  | Or    of t list

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
  | True -> "\u{22A4}"
  | False -> "\u{22A5}"
  | Pred p -> Qpred.to_string p
  | Not f' -> "\u{00AC} " ^ (to_string_aux f')
  | And fs -> "(" ^ String.concat " \u{2227} " (List.map to_string_aux fs) ^ ")"
  | Or fs -> "(" ^ String.concat " \u{2228} " (List.map to_string_aux fs) ^ ")" 

let to_string (f : t) : string =
  to_string_aux @@ flatten f

let rec to_yojson_aux (f : t) : Yojson.Safe.t =
  match f with
  | True ->
    `Assoc [ ("type", `String "True") ]
  | False ->
    `Assoc [ ("type", `String "False") ]
  | Pred p ->
      `Assoc [ ("type", `String "Pred");
               ("pred", Qpred.to_yojson p)]
  | Not f' ->
    `Assoc [ ("type", `String "Not");
             ("formula", to_yojson_aux f') ]
  | And fs ->
    `Assoc [ ("type", `String "And");
             ("formulas", `List (List.map to_yojson_aux fs)) ]
  | Or fs ->
    `Assoc [ ("type", `String "Or");
             ("formulas", `List (List.map to_yojson_aux fs)) ]

let to_yojson (f : t) : Yojson.Safe.t =
  to_yojson_aux @@ flatten f

let sequence_results (results: ('a, string) result list) : ('a list, string) result =
  let rec aux acc = function
  | [] -> Ok (List.rev acc)
  | (Ok x) :: xs -> aux (x :: acc) xs
  | (Error m) :: _ -> Error m in
  aux [] results

let rec of_yojson (json : Yojson.Safe.t) : (t, string) result =
  try
    let json_to_string = Yojson.Safe.Util.to_string in
    match json with
    | `Assoc _ ->
      let formula_type = member "type" json |> json_to_string in
      (match formula_type with
      | "True" ->
        Ok True
      | "False" ->
        Ok False
      | "Pred" ->
        (match Qpred.of_yojson (member "pred" json) with
         | Ok p -> Ok (Pred p)
         | Error e -> Error e)
      | "Not" ->
        let f = member "formula" json |> of_yojson in
        (match f with
        | Ok f -> Ok (Not f)
        | Error m -> Error m)
      | "And" ->
        let fs = member "formulas" json |> to_list |> List.map of_yojson |> sequence_results in
        (match fs with
        | Ok fs -> Ok (And fs)
        | Error m -> Error m)
      | "Or" ->
        let fs = member "formulas" json |> to_list |> List.map of_yojson |> sequence_results in
        (match fs with
        | Ok fs -> Ok (Or fs)
        | Error m -> Error m)
      | s ->
        Error ("Unknown formula type: " ^ s))
    | _ ->
      Error "Expected a JSON object at the top level"
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)