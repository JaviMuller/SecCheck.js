open Yojson.Safe.Util

module LocSet = Set.Make(struct
  type t = Ploc.t
  let compare = compare
end)

module LocValSet = Set.Make(struct
  type t = Ploc.t * Value.t
  let compare = compare
end)

module LocStrSet = Set.Make(struct
  type t = Ploc.t * string
  let compare = compare
end)

module LocLocSet = Set.Make(struct
  type t = Ploc.t * Ploc.t
  let compare = compare
end)

type pred = {
  base_taint   : LocSet.t;
  sink         : LocSet.t;
  sanitizer    : LocSet.t;
  has_value    : LocValSet.t;
  has_fun_name : LocStrSet.t;
  depends      : LocLocSet.t;
  tainted      : LocSet.t;
  same_value   : LocLocSet.t;
}

let sequence_results (results: ('a, string) result list) : ('a list, string) result =
  let rec aux acc = function
    | [] -> Ok (List.rev acc)
    | (Ok x) :: xs -> aux (x :: acc) xs
    | (Error m) :: _ -> Error m in
  aux [] results

let pred_to_yojson (p : pred) : Yojson.Safe.t =
  let lset_to_yojson (s : LocSet.t) : Yojson.Safe.t =
    `List (List.map (fun l -> `List [Ploc.to_yojson l]) (LocSet.elements s)) in
  let lvset_to_yojson (s : LocValSet.t) : Yojson.Safe.t =
    `List (List.map (fun (l, v) -> `List [Ploc.to_yojson l; Value.to_yojson v]) (LocValSet.elements s)) in
  let lsset_to_yojson (s : LocStrSet.t) : Yojson.Safe.t =
    `List (List.map (fun (l, s) -> `List [Ploc.to_yojson l; `String s]) (LocStrSet.elements s)) in
  let llset_to_yojson (s : LocLocSet.t) : Yojson.Safe.t =
    `List (List.map (fun (l1, l2) -> `List [Ploc.to_yojson l1; Ploc.to_yojson l2]) (LocLocSet.elements s)) in
  `Assoc [ ("base_taint"  , lset_to_yojson p.base_taint);
           ("sink"        , lset_to_yojson p.sink);
           ("sanitizer"   , lset_to_yojson p.sanitizer);
           ("has_value"   , lvset_to_yojson p.has_value);
           ("has_fun_name", lsset_to_yojson p.has_fun_name);
           ("depends"     , llset_to_yojson p.depends);
           ("tainted"     , lset_to_yojson p.tainted);
           ("same_value"  , llset_to_yojson p.same_value) ]

let pred_of_yojson (json : Yojson.Safe.t) : (pred, string) result =
  let lset_of_yojson (json : Yojson.Safe.t list list) : (LocSet.t, string) result =
    List.fold_left (fun acc es ->
      match acc, sequence_results @@ List.map Ploc.of_yojson es with
      | Ok acc, Ok [e] ->
        Ok (LocSet.add e acc)
      | Error e, _ -> Error e
      | _, Ok _ -> Error "Expected a single element"
      | _, Error e -> Error e) (Ok LocSet.empty) json in
  let lvset_of_yojson (json : Yojson.Safe.t list list) : (LocValSet.t, string) result =
    List.fold_left (fun acc json' ->
      match acc, json' with
      | Ok acc, [l ; v] ->
        (match Ploc.of_yojson l, Value.of_yojson v with
         | Ok l, Ok v ->
           Ok (LocValSet.add (l, v) acc)
         | Error e, _ | _, Error e -> Error e)
      | Ok _, _ ->
        Error "Unexpected location-value pair"
      | Error e, _ -> Error e) (Ok LocValSet.empty) json in
  let lsset_of_yojson (json : Yojson.Safe.t list list) : (LocStrSet.t, string) result =
    List.fold_left (fun acc json' ->
      match acc, json' with
      | Ok acc, [l; s] ->
        (match Ploc.of_yojson l, Yojson.Safe.Util.to_string s with
         | Ok l, s ->
           Ok (LocStrSet.add (l, s) acc)
         | Error e, _ -> Error e)
      | Ok _, _ ->
        Error "Unexpected location-string pair"
      | Error e, _ -> Error e) (Ok LocStrSet.empty) json in
  let llset_of_yojson (json : Yojson.Safe.t list list) : (LocLocSet.t, string) result = 
    List.fold_left (fun s json' ->
      match s, json' with
      | Ok s, [l1; l2] ->
        (match Ploc.of_yojson l1, Ploc.of_yojson l2 with
         | Ok l1, Ok l2 ->
           Ok (LocLocSet.add (l1, l2) s)
         | Error e, _ | _, Error e -> Error e)
      | Ok _, _ ->
        Error "Unexpected location-location pair"
      | Error e, _ -> Error e) (Ok LocLocSet.empty) json in
  try
    match json with
    | `Assoc _ ->
        let base_taint = member "base_taint" json
            |> to_list
            |> List.map to_list
            |> lset_of_yojson in
        let sink = member "sink" json
            |> to_list
            |> List.map to_list
            |> lset_of_yojson in
        let sanitizer = member "sanitizer" json
            |> to_list
            |> List.map to_list
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
            |> List.map to_list
            |> lset_of_yojson in
        let same_value = member "same_value" json
            |> to_list
            |> List.map to_list
            |> llset_of_yojson in
        (match base_taint, sink, sanitizer, has_value, has_fun_name, depends, tainted, same_value with
         | Ok base_taint, Ok sink, Ok sanitizer, Ok has_value, Ok has_fun_name, Ok depends, Ok tainted, Ok same_value ->
           Ok {
             base_taint;
             sink;
             sanitizer;
             has_value;
             has_fun_name;
             depends;
             tainted;
             same_value
           }
         | Error e, _, _, _, _, _, _, _
         | _, Error e, _, _, _, _, _, _
         | _, _, Error e, _, _, _, _, _
         | _, _, _, Error e, _, _, _, _
         | _, _, _, _, Error e, _, _, _
         | _, _, _, _, _, Error e, _, _
         | _, _, _, _, _, _, Error e, _
         | _, _, _, _, _, _, _, Error e -> Error e)
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
        | Error e -> failwith(e)) (Hashtbl.create 1000) trace_json in
      Ok { locs; preds; states; acc_states; trace }
    | _ -> Error "Invalid JSON format"
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)