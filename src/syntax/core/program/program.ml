open Yojson.Safe.Util

type t = {
  locs   : string list;
  preds  : ((string * Expr.conc list), unit) Hashtbl.t;
  states : int;
  final  : int list;
  trace  : (int, Transition.t list) Hashtbl.t; (* Maybe save them indexed by from in hashtbl/map? *)
}

let is_final({ final; _ } : t) (s : int) : bool =
  List.exists (fun f -> f = s) final

let state_transitions ({ trace; _ } : t) (s : int) : Transition.t list =
  match Hashtbl.find_opt trace s with
  | Some ts -> ts
  | None -> []

let num_states ({ states; _ } : t) : int =
  states

let has_pred ({ preds; _ } : t) (name : string) (args : Expr.conc list) : bool =
  Hashtbl.mem preds (name, args)

let to_yojson (p : t) : Yojson.Safe.t =
  let preds_to_yojson =
    Hashtbl.fold (fun (name, args) _ acc ->
      let args_json = `List (List.map Expr.conc_to_yojson args) in
      `Assoc [ ("name", `String name);
               ("args", args_json) ] :: acc) p.preds [] in
  `Assoc [ ("locs", `List (List.map (fun x -> `String x) p.locs));
           ("preds", `List preds_to_yojson);
           ("states", `Int p.states);
           ("final", `List (List.map (fun x -> `Int x) p.final));
           ("trace", `List (List.map Transition.to_yojson @@ Hashtbl.fold (fun _ v acc -> acc @ v) p.trace [])) ]

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  let json_to_string = Yojson.Safe.Util.to_string in
  let sequence_results (results: ('a, string) result list) : ('a list, string) result =
    let rec aux acc = function
      | [] -> Ok (List.rev acc)
      | (Ok x) :: xs -> aux (x :: acc) xs
      | (Error m) :: _ -> Error m in
    aux [] results in
  try
    match json with
    | `Assoc _ ->
      let locs = member "locs" json |> to_list |> List.map json_to_string in
      let preds_json = member "preds" json |> to_list in
      let preds = List.fold_left (fun acc pred_json ->
        let name = member "name" pred_json |> json_to_string in
        let args = member "args" pred_json |> to_list |> List.map Expr.conc_of_yojson |> sequence_results in
        match args with
        | Ok args -> Hashtbl.add acc (name, args) () ; acc
        | Error e -> failwith e) (Hashtbl.create 1000) preds_json in
      let states = member "states" json |> to_int in
      let final = member "final" json |> to_list |> List.map to_int in
      let trace_json = member "trace" json |> to_list in
      let trace = List.fold_left (fun acc t_json ->
        match Transition.of_yojson t_json with
        | Ok t ->
          if Hashtbl.mem acc t.src then
            let prev_trans = Hashtbl.find acc t.src in
            Hashtbl.replace acc t.src (t :: prev_trans)
          else
            Hashtbl.add acc t.src [t];
          acc
        | Error e -> failwith(e)) (Hashtbl.create 1000) trace_json in
      Ok { locs; preds; states; final; trace }
    | _ -> Error "Invalid JSON format"
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)