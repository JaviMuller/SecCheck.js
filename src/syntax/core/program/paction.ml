open Yojson.Safe.Util

type t =
  | FuncCall        of (Ploc.t * Ploc.t list list)
  | FuncRet         of (Ploc.t * Ploc.t list)
  | PropAssign      of (Ploc.t list * Ploc.t list * Ploc.t list)
  | PropLookup      of (Ploc.t list * Ploc.t list * Ploc.t list)

let to_string (a : t) : string =
  let locs_str = List.map Ploc.to_string in
  match a with
  | FuncCall (name, args) -> 
    let callee = Ploc.to_string name in
    let args = List.map (fun x -> "{" ^ String.concat ", " (locs_str x) ^ "}") args in
    callee ^ "(" ^ String.concat ", " args ^ ")"
  | FuncRet  (name, ret) ->
    let callee = Ploc.to_string name in
    let ret = "{" ^ String.concat ", " (locs_str ret) ^ "}" in
    callee ^ " -> " ^ ret
  | PropAssign (obj, prop, v) ->
    let obj = "{" ^ String.concat ", " (locs_str obj) ^ "}" in
    let prop = "{" ^ String.concat ", " (locs_str prop) ^ "}" in
    let v = "{" ^ String.concat ", " (locs_str v) ^ "}" in
    obj ^ "[" ^ prop ^ "] := " ^ v
  | PropLookup (var, obj, prop) ->
    let var = "{" ^ String.concat ", " (locs_str var) ^ "}" in
    let obj = "{" ^ String.concat ", " (locs_str obj) ^ "}" in
    let prop = "{" ^ String.concat ", " (locs_str prop) ^ "}" in
    var ^ " := " ^ obj ^ "[" ^ prop ^ "]"

let to_yojson (a : t) : Yojson.Safe.t =
  let loc_to_json_string x = `String (Ploc.to_string x) in
  match a with
  | FuncCall (name, args) ->
    `Assoc [ ("type", `String "FuncCall");
             ("callee", loc_to_json_string name);
             ("args", `List (List.map 
                              (fun arg -> `List (List.map loc_to_json_string arg))
                              args)) ]
  | FuncRet (name, ret) ->
    `Assoc [ ("type", `String "FuncRet");
             ("callee", loc_to_json_string name);
             ("ret", `List (List.map loc_to_json_string ret)) ]
  | PropAssign (obj, prop, value) ->
    `Assoc [ ("type", `String "PropAssign");
             ("object", `List (List.map loc_to_json_string obj));
             ("property", `List (List.map loc_to_json_string prop));
             ("value", `List (List.map loc_to_json_string value)) ]
  | PropLookup (targ, obj, prop) ->
    `Assoc [ ("type", `String "PropLookup");
             ("target", `List (List.map loc_to_json_string targ));
             ("object", `List (List.map loc_to_json_string obj));
             ("property", `List (List.map loc_to_json_string prop))]

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  let json_to_string = Yojson.Safe.Util.to_string in
  let json_to_loc x = Ploc.Loc (Yojson.Safe.Util.to_string x) in
  try
    match json with
    | `Assoc _ ->
      let action_type = member "type" json |> json_to_string in
      (
        match action_type with
        | "FuncCall" ->
          let callee = member "callee" json |> json_to_loc in
          let args = member "args" json |> to_list |> List.map to_list |> List.map @@ List.map json_to_loc in
          Ok (FuncCall (callee, args))
        | "FuncRet" ->
          let callee = member "callee" json |> json_to_loc in
          let ret = member "ret" json |> to_list |> List.map json_to_loc in
          Ok (FuncRet (callee, ret))
        | "PropAssign" ->
          let obj = member "object" json |> to_list |> List.map json_to_loc in
          let prop = member "property" json |> to_list |> List.map json_to_loc in
          let value = member "value" json |> to_list |> List.map json_to_loc in
          Ok (PropAssign (obj, prop, value))
        | "PropLookup" ->
          let targ = member "target" json |> to_list |> List.map json_to_loc in
          let obj = member "object" json |> to_list |> List.map json_to_loc in
          let prop = member "property" json |> to_list |> List.map json_to_loc in
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
    
type eps =
  | Epsilon
  | Action of t

let eps_to_yojson (a : eps) : Yojson.Safe.t =
  match a with
  | Epsilon ->
    `Assoc [ ("type", `String "Epsilon") ]
  | Action a ->
    to_yojson a

let eps_of_yojson (json : Yojson.Safe.t) : (eps, string) result =
  let json_to_string = Yojson.Safe.Util.to_string in
  try
    match json with
    | `Assoc _ ->
      let action_type = member "type" json |> json_to_string in
      (match action_type, of_yojson json with
       | "Epsilon", _ ->
         Ok Epsilon
       | _, Ok a ->
         Ok (Action a)
       | _, Error e ->
         Error e)
    | _ ->
      Error "Expected a JSON object at the top level"
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)