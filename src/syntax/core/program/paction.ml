open Yojson.Safe.Util

type t = 
  | FuncCall        of (string list * string list list)
  | PropAssign      of (string list * string list * string list)
  | PropLookup      of (string list * string list * string list)

let to_string (a : t) : string =
  match a with
  | FuncCall (name, args) -> 
    let callee = "{" ^ String.concat ", " name ^ "}" in
    let args = List.map (fun x -> "{" ^ String.concat ", " x ^ "}") args in
    callee ^ "(" ^ String.concat ", " args ^ ")"
  | PropAssign (obj, prop, v) ->
    let obj = "{" ^ String.concat ", " obj ^ "}" in
    let prop = "{" ^ String.concat ", " prop ^ "}" in
    let v = "{" ^ String.concat ", " v ^ "}" in
    obj ^ "[" ^ prop ^ "] := " ^ v
  | PropLookup (var, obj, prop) ->
    let var = "{" ^ String.concat ", " var ^ "}" in
    let obj = "{" ^ String.concat ", " obj ^ "}" in
    let prop = "{" ^ String.concat ", " prop ^ "}" in
    var ^ " := " ^ obj ^ "[" ^ prop ^ "]"

let to_yojson (a : t) : Yojson.Safe.t =
  let to_json_string x = `String x in
  match a with
  | FuncCall (name, args) ->
    `Assoc [ ("type", `String "FuncCall");
             ("callee", `List (List.map to_json_string name));
             ("args", `List (List.map 
                              (fun arg -> `List (List.map to_json_string arg))
                              args)) ]
  | PropAssign (obj, prop, value) ->
    `Assoc [ ("type", `String "PropAssign");
             ("object", `List (List.map to_json_string obj));
             ("property", `List (List.map to_json_string prop));
             ("value", `List (List.map to_json_string value)) ]
  | PropLookup (targ, obj, prop) ->
    `Assoc [ ("type", `String "PropLookup");
             ("target", `List (List.map to_json_string targ));
             ("object", `List (List.map to_json_string obj));
             ("property", `List (List.map to_json_string prop))]

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  let json_to_string = Yojson.Safe.Util.to_string in
  try
    match json with
    | `Assoc _ ->
      let action_type = member "type" json |> json_to_string in
      (
        match action_type with
        | "FuncCall" ->
          let callee = member "callee" json |> to_list |> List.map json_to_string in
          let args = member "args" json |> to_list |> List.map to_list |> List.map @@ List.map json_to_string in
          Ok (FuncCall (callee, args))
        | "PropAssign" ->
          let obj = member "object" json |> to_list |> List.map json_to_string in
          let prop = member "property" json |> to_list |> List.map json_to_string in
          let value = member "value" json |> to_list |> List.map json_to_string in
          Ok (PropAssign (obj, prop, value))
        | "PropLookup" ->
          let targ = member "target" json |> to_list |> List.map json_to_string in
          let obj = member "object" json |> to_list |> List.map json_to_string in
          let prop = member "property" json |> to_list |> List.map json_to_string in
          Ok (PropLookup (targ, obj, prop))
        | s ->
          Error ("Unknown action type: " ^ s))
    | _ ->
      Error "Expected a JSON object at the top level"
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)
    