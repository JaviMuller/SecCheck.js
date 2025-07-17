open Yojson.Safe.Util

type t = 
  {
    prop_name    : string;
    states       : int;
    init_state   : int;
    acc_states   : int list;
    transitions  : (int, Qtransition.t list) Hashtbl.t;
  }

let sequence_results (results: ('a, string) result list) : ('a list, string) result =
  let rec aux acc = function
  | [] -> Ok (List.rev acc)
  | (Ok x) :: xs -> aux (x :: acc) xs
  | (Error m) :: _ -> Error m in
  aux [] results
  
let get_prop_name ({ prop_name; _ } : t) : string =
  prop_name

let get_states ({ states; _ } : t) : int =
  states

let get_init_state ({ init_state; _ } : t) : int =
  init_state

let get_acc_states ({ acc_states; _ } : t) : int list =
  acc_states

let is_acc_state ({ acc_states; _ } : t) (s : int) : bool =
  List.mem s acc_states

let get_transitions ({ transitions; _ } : t) : (int, Qtransition.t list) Hashtbl.t =
  transitions

let get_applicable_trs ({ transitions; _ } : t) (s : int) : Qtransition.t list option=
  Hashtbl.find_opt transitions s

let load_file_contents (f: string) : string =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s


let of_ltlf2dfa (name : string) (subst : (string, Qaction.t) Hashtbl.t) (json: Yojson.Safe.t) : (t, string) result =
  let transition_of_json (json : Yojson.Safe.t) : (Qtransition.t, string) result =
    try
      match json with
      | `Assoc _ ->
        let src = member "src" json |> to_int in
        let dest = member "dest" json |> to_int in
        let lbl_str = member "lbl" json |> to_string in
        let lbl =
          if lbl_str = "*"
            then Qaction.Any
            else Hashtbl.find subst lbl_str in
        Ok({
             src = src;
             dest = dest;
             lbl = lbl
           })
      | _ -> Error "Expected a JSON object at the top level"
    with
    | Type_error (msg, _) ->
      Error ("Type error during parsing: " ^ msg)
    | e ->
      Error ("Unexpected error: " ^ Printexc.to_string e) in

  try
    match json with
    | `Assoc _ ->
      let num_states = member "states" json |> to_int in
      let init_state = member "init_state" json |> to_int in
      let acc_states = member "acc_states" json |> to_list |> List.map to_int in
      let transitions = member "transitions" json |> to_list |> List.map transition_of_json |> sequence_results in
      (match transitions with
       | Ok trs ->
         let transition_table = Hashtbl.create 50 in
         List.fold_left (fun _ tr ->
          let src = Qtransition.get_src tr in
          (match Hashtbl.find_opt transition_table src with
           | Some trs -> Hashtbl.replace transition_table src (tr :: trs)
           | None -> Hashtbl.add transition_table src [tr])) () trs;
         Ok {
           prop_name = name;
           states = num_states;
           init_state = init_state;
           acc_states = acc_states;
           transitions = transition_table
         }
       | Error e -> Error e
      )
      | _ -> Error "Expected a JSON object at the top level"
    with
    | Type_error (msg, _) ->
      Error ("Type error during parsing: " ^ msg)
    | e -> 
      Error ("Unexpected error: " ^ Printexc.to_string e)

let from_trcf ?(name="Vulnerability") (f : Trcformula.t) : (t, string) result =
  let lang = match f with
             | FutureFormula _ -> "ltlf"
             | PastFormula _ -> "ppltl" in
  let subst = Hashtbl.create 20 in
  let tmp_file = "out.json" in
  Sys.chdir("/home/jmuller/GitHub/TrustedCPG");
  let cmd = Filename.quote_command (String.concat Filename.dir_sep [Sys.getcwd (); ".venv"; "bin"; "python"])
            ~stdout:tmp_file ["ltl2dfa.py"; "-l"; lang; "-s"; Trcformula.to_ltlf2dfa subst f] in
  let _ = Sys.command cmd in
  let json_str = load_file_contents(tmp_file) in
  of_ltlf2dfa name subst @@ Yojson.Safe.from_string json_str

let to_yojson (qa : t) : Yojson.Safe.t =
  `Assoc [ ("prop_name", `String qa.prop_name);
           ("states", `Int qa.states);
           ("init_state", `Int qa.init_state);
           ("acc_states", `List (List.map (fun x -> `Int x) qa.acc_states));
           ("transitions", `List (Hashtbl.fold 
                                  (fun _ v acc -> List.append (List.map Qtransition.to_yojson v) 
                                                  acc) 
                                  qa.transitions [])) ]

let of_yojson (json : Yojson.Safe.t) : (t, string) result =
  try
    match json with
    | `Assoc _ ->
      let prop_name = member "prop_name" json |> to_string in
      let states = member "states" json |> to_int in
      let init_state = member "init_state" json |> to_int in
      let acc_states = member "acc_states" json |> to_list |> List.map to_int in
      let transitions = Hashtbl.create 50 in
      let trs_json = member "transitions" json
        |> to_list
        |> List.map Qtransition.of_yojson
        |> sequence_results in
      (match trs_json with
       | Ok trs ->
         List.fold_left (fun _ tr -> 
                          let src = Qtransition.get_src tr in
                          match Hashtbl.find_opt transitions src with
                          | Some trs -> Hashtbl.replace transitions src @@ tr :: trs
                          | None -> Hashtbl.add transitions src [tr]
                        ) () trs;
       Ok {
         prop_name   = prop_name;
         states      = states;
         init_state  = init_state;
         acc_states  = acc_states;
         transitions = transitions
       }
      | Error e -> Error e)
    | _ ->
      Error "Expected a JSON object at the top level"
  with
  | Type_error (msg, _) ->
    Error ("Type error during parsing: " ^ msg)
  | e ->
    Error ("Unexpected error: " ^ Printexc.to_string e)
