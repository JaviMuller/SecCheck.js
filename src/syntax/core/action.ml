open Yojson.Safe.Util

type t = 
  | FuncCall of (string * string list)
  | FuncCallWithArg of (string * string)
  | FuncCallAnyArgs of string
  | PropAssign of (string * string * string)
  | PropLookup of (string * string * string)

let to_string (a : t) : string =
  match a with
  | FuncCall (name, args) -> name ^ "(" ^ String.concat ", " args ^ ")"
  | FuncCallWithArg (name, arg) -> name ^ "(..." ^ arg ^ ")"
  | FuncCallAnyArgs (name) -> name ^ "(...)"
  | PropAssign (obj, prop, v) -> obj ^ "[" ^ prop ^ "]" ^ " := " ^ v
  | PropLookup (var, obj, prop) -> var ^ " := " ^ obj ^ "[" ^ prop ^ "]"

let to_yojson (a : t) : Yojson.Safe.t =
  let action = match a with
  | FuncCall (name, args) ->
    `Assoc [ ("type", `String "FuncCall");
             ("callee", `String name);
             ("args", `List (List.map (fun x -> `String x) args)) ]
  | FuncCallWithArg (name, arg) ->
    `Assoc [ ("type", `String "FuncCallWithArg");
             ("callee", `String name);
             ("arg", `String arg) ]
  | FuncCallAnyArgs name ->
    `Assoc [ ("type", `String "FuncCallAnyArgs");
             ("callee", `String name) ]
  | PropAssign (obj, prop, v) ->
    `Assoc [ ("type", `String "PropAssign");
             ("object", `String obj);
             ("property", `String prop);
             ("value", `String v) ]
  | PropLookup (targ, obj, prop) ->
    `Assoc [ ("type", `String "PropLookup");
             ("target", `String targ);
             ("object", `String obj);
             ("property", `String prop)] in
  `Assoc [ ("type", `String "TraceAction");
           ("action", action) ]

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  try
    let json_to_string = Yojson.Safe.Util.to_string in
    match json with
    | `Assoc _ ->
      let top_type = member "type" json |> json_to_string in 
      if top_type <> "TraceAction" then
        Error("Expected type TraceAction but got: " ^ top_type)
      else
        let action_json = member "action" json in
        begin
          match action_json with
          | `Assoc _ ->
            let action_type = member "type" action_json |> json_to_string in
            (
              match action_type with
              | "FuncCall" ->
                let callee = member "callee" action_json |> json_to_string in
                let args = member "args" action_json |> to_list |> List.map json_to_string in
                Ok (FuncCall (callee, args))
              | "FuncCallWithArg" ->
                let callee = member "callee" action_json |> json_to_string in
                let arg = member "arg" action_json |> json_to_string in
                Ok (FuncCallWithArg (callee, arg))
              | "FuncCallAnyArgs" ->
                let callee = member "callee" action_json |> json_to_string in
                Ok (FuncCallAnyArgs callee)
              | "PropAssign" ->
                let obj = member "object" action_json |> json_to_string in
                let prop = member "property" action_json |> json_to_string in
                let value = member "value" action_json |> json_to_string in
                Ok (PropAssign (obj, prop, value))
              | "PropLookup" ->
                let targ = member "target" action_json |> json_to_string in
                let obj = member "object" action_json |> json_to_string in
                let prop = member "property" action_json |> json_to_string in
                Ok (PropLookup (targ, obj, prop))
              | s ->
                Error ("Unknown action type: " ^ s))
          | _ ->
            Error "Expected the \"action\" field to be an object"
        end
    | _ ->
      Error "Expected a JSON object at the top level"
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)
    