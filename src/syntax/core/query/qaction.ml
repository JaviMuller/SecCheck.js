open Yojson.Safe.Util

type t = 
  | FuncCall        of (Qvar.t * Qvar.t list)
  | FuncCallWithArg of (Qvar.t * Qvar.t)
  | FuncCallAnyArgs of Qvar.t
  | PropAssign      of (Qvar.t * Qvar.t * Qvar.t)
  | PropLookup      of (Qvar.t * Qvar.t * Qvar.t)
  | Any

let to_string (a : t) : string =
  let qv_str = Qvar.to_string in
  match a with
  | FuncCall (callee, args) -> 
    (qv_str callee) ^ "(" ^ (String.concat ", " @@ List.map qv_str args) ^ ")"
  | FuncCallWithArg (callee, arg) ->
    (qv_str callee) ^ "(..." ^ (qv_str arg) ^ ")"
  | FuncCallAnyArgs (callee) ->
    (qv_str callee) ^ "(...)"
  | PropAssign (obj, prop, v) ->
    (qv_str obj) ^ "[" ^ (qv_str prop) ^ "]" ^ " := " ^ (qv_str v)
  | PropLookup (var, obj, prop) ->
    (qv_str var) ^ " := " ^ (qv_str obj) ^ "[" ^ (qv_str prop) ^ "]"
  | Any ->
    "*"

let to_yojson (a : t) : Yojson.Safe.t =
  let qv_json = Qvar.to_yojson in
  match a with
  | FuncCall (callee, args) ->
    `Assoc [ ("type", `String "FuncCall");
             ("callee", qv_json callee);
             ("args", `List (List.map qv_json args)) ]
  | FuncCallWithArg (callee, arg) ->
    `Assoc [ ("type", `String "FuncCallWithArg");
             ("callee", qv_json callee);
             ("arg", qv_json arg) ]
  | FuncCallAnyArgs callee ->
    `Assoc [ ("type", `String "FuncCallAnyArgs");
             ("callee", qv_json callee) ]
  | PropAssign (obj, prop, v) ->
    `Assoc [ ("type", `String "PropAssign");
             ("object", qv_json obj);
             ("property", qv_json prop);
             ("value", qv_json v) ]
  | PropLookup (targ, obj, prop) ->
    `Assoc [ ("type", `String "PropLookup");
             ("target", qv_json targ);
             ("object", qv_json obj);
             ("property", qv_json prop)]
  | Any -> `Assoc [ ("type", `String "Any") ]

let sequence_results (results: ('a, string) result list) : ('a list, string) result =
  let rec aux acc = function
  | [] -> Ok (List.rev acc)
  | (Ok x) :: xs -> aux (x :: acc) xs
  | (Error m) :: _ -> Error m in
  aux [] results

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  try
    let qv_of_json = Qvar.of_yojson in
    match json with
    | `Assoc _ ->
      let action_type = member "type" json |> Yojson.Safe.Util.to_string in
      (
        match action_type with
        | "FuncCall" ->
          let callee = member "callee" json |> qv_of_json in
          let args = member "args" json |> to_list |> List.map qv_of_json |> sequence_results in
          (match callee, args with
          | Ok callee, Ok args ->
            Ok (FuncCall (callee, args))
          | Error e, _ | _, Error e ->
            Error e)
        | "FuncCallWithArg" ->
          let callee = member "callee" json |> qv_of_json in
          let arg = member "arg" json |> qv_of_json in
          (match callee, arg with
          | Ok callee, Ok arg ->
            Ok (FuncCallWithArg (callee, arg))
          | Error e, _ | _, Error e ->
            Error e)
        | "FuncCallAnyArgs" ->
          let callee = member "callee" json |> qv_of_json in
          (match callee with
          | Ok callee ->
            Ok (FuncCallAnyArgs callee)
          | Error e -> Error e)
        | "PropAssign" ->
          let obj = member "object" json |> qv_of_json in
          let prop = member "property" json |> qv_of_json in
          let value = member "value" json |> qv_of_json in
          (match obj, prop, value with
          | Ok obj, Ok prop, Ok value ->
            Ok (PropAssign (obj, prop, value))
          | Error e , _, _ | _, Error e, _ | _, _, Error e ->
            Error e)
        | "PropLookup" ->
          let targ = member "target" json |> qv_of_json in
          let obj = member "object" json |> qv_of_json in
          let prop = member "property" json |> qv_of_json in
          (match targ, obj, prop with
          | Ok targ, Ok obj, Ok prop ->
            Ok (PropLookup (targ, obj, prop))
          | Error e, _, _ | _, Error e, _ | _, _, Error e ->
            Error e)
        | "Any" -> Ok (Any)
        | s ->
          Error ("Unknown action type: " ^ s))
    | _ ->
      Error "Expected a JSON object at the top level"
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)
