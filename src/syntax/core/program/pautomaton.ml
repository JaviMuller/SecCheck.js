open Yojson.Safe.Util

module LocSet = Set.Make(struct
  type t = Ploc.t
  let compare = compare
end)

module LocMap = Map.Make(struct
  type t = Ploc.t
  let compare = compare
end)

type pred = {
  base_taint   : LocSet.t;
  sink         : LocSet.t;
  sanitizer    : LocSet.t;
  has_value    : Value.abs LocMap.t;
  has_fun_name : string LocMap.t;
  depends      : Ploc.t list LocMap.t;
  tainted      : LocSet.t;
}

let is_base_taint (p : pred) (l : Ploc.t) : bool =
  LocSet.mem l p.base_taint

let is_sink (p : pred) (l : Ploc.t) : bool =
  LocSet.mem l p.sink

let is_sanitizer (p : pred) (l : Ploc.t) : bool =
  LocSet.mem l p.sink

let has_value (p : pred) (l : Ploc.t) (v : Value.t) : pred option =
  match LocMap.find_opt l p.has_value with
  | Some Many ->
    Some p
  | Some Val v' ->
    if v' = v 
      then Some p
    else None 
  | Some Any ->
    let new_has_value = LocMap.update l (fun _ : Value.abs option -> Some (Val v)) p.has_value in
    Some { p with has_value = new_has_value }
  | None -> None

let has_fun_name (p : pred) (l : Ploc.t) (name : string) : bool =
  match LocMap.find_opt l p.has_fun_name with
  | Some v -> v = name
  | None -> false

let depends (p : pred) (l1 : Ploc.t) (l2 : Ploc.t) : bool =
  match LocMap.find_opt l1 p.depends with
  | Some ls -> List.mem l2 ls
  | None -> false

let is_tainted (p : pred) (l : Ploc.t) : bool =
  LocSet.mem l p.tainted

let same_value (p : pred) (l1 : Ploc.t) (l2 : Ploc.t) : pred option =
  let v1 = LocMap.find_opt l1 p.has_value in
  let v2 = LocMap.find_opt l2 p.has_value in
  match v1, v2 with
  | Some (Val v1), _ ->
    has_value p l2 v1
  | _, Some(Val v2) ->
    has_value p l1 v2
  | None, _ | _, None -> None
  | _, _ -> Some p  

let sequence_results (results: ('a, string) result list) : ('a list, string) result =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | (Ok x) :: xs -> aux (x :: acc) xs
    | (Error m) :: _ -> Error m in
  aux [] results

let pred_to_yojson (p : pred) : Yojson.Safe.t =
  let lset_to_yojson (s : LocSet.t) : Yojson.Safe.t =
    `List (List.map (fun l -> Ploc.to_yojson l) (LocSet.elements s)) in
  let lvset_to_yojson (m : Value.abs LocMap.t) : Yojson.Safe.t =
    `List (List.map (fun (l, v) -> `List [Ploc.to_yojson l; Value.abs_to_yojson v]) (LocMap.bindings m)) in
  let lsset_to_yojson (s : string LocMap.t) : Yojson.Safe.t =
    `List (List.map (fun (l, s) -> `List [Ploc.to_yojson l; `String s]) (LocMap.bindings s)) in
  let llset_to_yojson (s : Ploc.t list LocMap.t) : Yojson.Safe.t =
    `List (List.concat 
      (List.map (fun (l1, l2s) -> 
        List.map (fun l2 -> `List [Ploc.to_yojson l1; Ploc.to_yojson l2]) l2s)
        (LocMap.bindings s))) in
  `Assoc [ ("base_taint"  , lset_to_yojson p.base_taint);
           ("sink"        , lset_to_yojson p.sink);
           ("sanitizer"   , lset_to_yojson p.sanitizer);
           ("has_value"   , lvset_to_yojson p.has_value);
           ("has_fun_name", lsset_to_yojson p.has_fun_name);
           ("depends"     , llset_to_yojson p.depends);
           ("tainted"     , lset_to_yojson p.tainted) ]

let pred_of_yojson (json : Yojson.Safe.t) : (pred, string) result =
  let lset_of_yojson (json : Yojson.Safe.t list) : (LocSet.t, string) result =
    List.fold_left (fun acc e ->
      match acc, Ploc.of_yojson e with
      | Ok acc, Ok e ->
        Ok (LocSet.add e acc)
      | Error e, _ -> Error e
      | _, Error e -> Error e) (Ok LocSet.empty) json in
  let lvset_of_yojson (json : Yojson.Safe.t list list) : (Value.abs LocMap.t, string) result =
    List.fold_left (fun acc json' ->
      match acc, json' with
      | Ok acc, [l ; v] ->
        (match Ploc.of_yojson l, Value.abs_of_yojson v with
         | Ok l, Ok v ->
           Ok (LocMap.add l v acc)
         | Error e, _ | _, Error e -> Error e)
      | Ok _, _ ->
        Error "Unexpected location-value pair"
      | Error e, _ -> Error e) (Ok LocMap.empty) json in
  let lsset_of_yojson (json : Yojson.Safe.t list list) : (string LocMap.t, string) result =
    List.fold_left (fun acc json' ->
      match acc, json' with
      | Ok acc, [l; s] ->
        (match Ploc.of_yojson l, Yojson.Safe.Util.to_string s with
         | Ok l, s ->
           Ok (LocMap.add l s acc)
         | Error e, _ -> Error e)
      | Ok _, _ ->
        Error "Unexpected location-string pair"
      | Error e, _ -> Error e) (Ok LocMap.empty) json in
  let llset_of_yojson (json : Yojson.Safe.t list list) : (Ploc.t list LocMap.t, string) result = 
    List.fold_left (fun s json' ->
      match s, json' with
      | Ok s, [l1; l2] ->
        (match Ploc.of_yojson l1, Ploc.of_yojson l2 with
         | Ok l1, Ok l2 ->
           Ok (LocMap.update l1 (fun ls ->
             match ls with
             | None -> Some [l2]
             | Some ls -> Some (l1 :: ls)) s)
         | Error e, _ | _, Error e -> Error e)
      | Ok _, _ ->
        Error "Unexpected location-location pair"
      | Error e, _ -> Error e) (Ok LocMap.empty) json in
  try
    match json with
    | `Assoc _ ->
        let base_taint = member "base_taint" json
            |> to_list
            |> lset_of_yojson in
        let sink = member "sink" json
            |> to_list
            |> lset_of_yojson in
        let sanitizer = member "sanitizer" json
            |> to_list
            |> lset_of_yojson in
        let has_value = member "has_value" json
            |> to_list
            |> List.map to_list
            |> lvset_of_yojson in
        let has_fun_name = member "has_fun_name" json
            |> to_list
            |> List.map to_list
            |> lsset_of_yojson in
        let depends = member "depends" json
            |> to_list
            |> List.map to_list
            |> llset_of_yojson in
        let tainted = member "tainted" json
            |> to_list
            |> lset_of_yojson in
        (match base_taint, sink, sanitizer, has_value, has_fun_name, depends, tainted with
         | Ok base_taint, Ok sink, Ok sanitizer, Ok has_value, Ok has_fun_name, Ok depends, Ok tainted ->
           Ok {
             base_taint;
             sink;
             sanitizer;
             has_value;
             has_fun_name;
             depends;
             tainted
           }
         | Error e, _, _, _, _, _, _ 
         | _, Error e, _, _, _, _, _ 
         | _, _, Error e, _, _, _, _
         | _, _, _, Error e, _, _, _
         | _, _, _, _, Error e, _, _
         | _, _, _, _, _, Error e, _
         | _, _, _, _, _, _, Error e -> Error e)
    | _ -> Error "Expected a JSON object at the top level"
  with
    | Type_error (msg, _) ->
      Error ("Type error during parsing: " ^ msg)
    | e ->
      Error ("Unexpected error: " ^ Printexc.to_string e)


type t = {
  locs        : Ploc.t list;
  preds       : pred;
  states      : int;
  acc_states  : int list;
  trace       : (int, Ptransition.t list) Hashtbl.t;
}

let get_preds (p : t) : pred = 
  p.preds

let is_acc_state ({ acc_states; _ } : t) (s : int) : bool =
  List.mem s acc_states

let state_transitions ({ trace; _ } : t) (s : int) : Ptransition.t list =
  match Hashtbl.find_opt trace s with
  | Some ts -> ts
  | None -> []

let num_states ({ states; _ } : t) : int =
  states

let to_yojson (p : t) : Yojson.Safe.t =
  `Assoc [ ("locs", `List (List.map (fun x -> `String (Ploc.to_string x)) p.locs));
           ("preds", pred_to_yojson p.preds);
           ("states", `Int p.states);
           ("acc_states", `List (List.map (fun x -> `Int x) p.acc_states));
           ("trace", `List (List.map Ptransition.to_yojson @@ Hashtbl.fold (fun _ v acc -> acc @ v) p.trace [])) ]

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  let json_to_string = Yojson.Safe.Util.to_string in
  try
    match json with
    | `Assoc _ ->
      let locs = member "locs" json |> to_list |> List.map (fun x -> Ploc.Loc (json_to_string x)) in
      let preds = match (member "preds" json |> pred_of_yojson) with
        | Ok pred -> pred
        | Error e -> failwith e in
      let states = member "states" json |> to_int in
      let acc_states = member "acc_states" json |> to_list |> List.map to_int in
      let trace_json = member "trace" json |> to_list in
      let trace = List.fold_left (fun acc t_json ->
        match Ptransition.of_yojson t_json with
        | Ok t ->
          if Hashtbl.mem acc t.src then
            let prev_trans = Hashtbl.find acc t.src in
            Hashtbl.replace acc t.src (t :: prev_trans)
          else
            Hashtbl.add acc t.src [t];
          acc
        | Error e -> failwith(e)) (Hashtbl.create 500) trace_json in
      Ok { locs; preds; states; acc_states; trace }
    | _ -> Error "Invalid JSON format"
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)
