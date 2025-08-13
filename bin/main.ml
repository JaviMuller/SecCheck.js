open SecCheckJSSyntax
open Stdlib

(* let get_query_json (q: Query.t) = 
  let qautom = Qautomaton.from_qry q in
  match qautom with
  | Ok q ->
    print_endline @@ Yojson.Safe.pretty_to_string @@ Qautomaton.to_yojson q
  | Error e -> failwith e *)

let cartesian_product (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  List.concat @@ List.map (fun e1 -> List.map (fun e2 -> (e1, e2)) l2) l1

let () = 
  if Array.length Sys.argv <> 3 then
    print_endline "Usage: dune exec trusted-cpg <program_json> <query_file>"
  else
    let program_ic = open_in Sys.argv.(1) in
    let program_json = In_channel.input_all program_ic in
    let pautom = Pautomaton.of_yojson @@ Yojson.Safe.from_string program_json in
    let query_file = Sys.argv.(2) in
    let query = SecCheckJSSemantics.Parsing.parse_query_from_file query_file in
    let qautom = Qautomaton.from_qry query in
    match pautom, qautom with
    | Ok p, Ok q ->
      let final_states = SecCheckJSSemantics.Verification.verify q p in
      (match final_states with
       | [] -> print_endline "No violationss were found!"
       | _  -> print_endline "Violations were found!";
               List.iter (fun s ->
                 let bindings = SecCheckJSSemantics.Verification.get_bindings s in
                 print_endline "Exploit: {";
                 print_string @@ SecCheckJSSemantics.Verification.bindings_to_string bindings;
                 print_endline "}";
                 print_endline "Traces: ";
                 let q_trc = List.rev s.q_trans in
                 let p_trc = List.rev s.p_trans in
                 let trcs = cartesian_product q_trc p_trc in
                 let _ = List.mapi (fun i (q_t, p_t) ->
                   print_endline @@ (string_of_int i) ^ ":";
                   print_endline @@ "  q: " ^ (Qtransition.to_string q_t);
                   print_endline @@ "  p: " ^ (Ptransition.to_string p_t)) trcs in
                 print_newline ()) final_states)
    | Error m, _ | _, Error m -> print_endline m
    (* match program with
    | Ok (p) ->
      let final_states = TcpgSemantics.Verification.verify query p in
      (match final_states with
       | [] -> print_endline "No violations were found!"
       | _ -> print_endline "Violations were found!";
              List.iter (fun s ->
                let bindings = TcpgSemantics.Verification.get_bindings s in
                print_endline "Exploit: {";
                print_string @@ TcpgSemantics.Verification.bindings_to_string bindings;
                print_endline "}") final_states
      )
    | Error m -> print_endline m *)

  (* let locs = ["l1"; "l2"; "l3"; "l4"; "l5"; "l6"; "l7"; "l8"] in
  let props : ((string * Expr.conc list), unit) Hashtbl.t = Hashtbl.create 10 in
  let prop_list : (string * Expr.conc list) list = 
    [ ("name", [Loc "l1"; Val (Value.String "f")]);
      ("name", [Loc "l2"; Val (Value.String "obj")]);
      ("name", [Loc "l3"; Val (Value.String "ls")]);
      ("name", [Loc "l4"; Val (Value.String "val")]);
      ("name", [Loc "l5"; Val (Value.String "i")]);
      ("name", [Loc "l6"; Val (Value.String "ls.*")]);
      ("name", [Loc "l7"; Val (Value.String "obj.*")]);
      ("name", [Loc "l8"; Val (Value.String "new_obj")]);
      ("name", [Loc "l9"; Val (Value.String "new_obj")]);
      ("tainted", [Loc "l2"]);
      ("tainted", [Loc "l3"]);
      ("tainted", [Loc "l4"]);
      ("tainted", [Loc "l6"]);
      ("tainted", [Loc "l7"]);
      ("tainted", [Loc "l9"]) ] in
  prop_list
    |> List.map (fun x -> (x, ()))
    |> List.to_seq
    |> Hashtbl.add_seq props;
  let states = 2 in
  let final = [2] in
  let t_list = [
    Transition.{src = 0; dest = 1; action = Paction.PropLookup (["l6"], ["l3"], ["l5"])}; (* aux_1 = ls[i] *)
    Transition.{src = 1; dest = 0; action = Paction.PropLookup (["l7"; "l9"], ["l2"], ["l6"])}; (* new_obj = obj[aux_1] - goto next iter *)
    Transition.{src = 1; dest = 2; action = Paction.PropLookup (["l7"; "l9"], ["l2"], ["l6"])}; (* new_obj = obj[aux_1] - exit loop *)
    Transition.{src = 1; dest = 0; action = Paction.PropAssign (["l8"; "l9"], ["l6"], ["l4"])}; (* new_obj[aux_1] = val - goto next iter *)
    Transition.{src = 1; dest = 2; action = Paction.PropAssign (["l8"; "l9"], ["l6"], ["l4"])}  (* new_obj[aux_1] = val - exit loop *)
  ] in
  let trace : (int, Transition.t list) Hashtbl.t = Hashtbl.create 10 in
  List.fold_left (fun _ t ->
    let src = Transition.get_src t in
    if Hashtbl.mem trace src then
      let ts = Hashtbl.find trace src in
      Hashtbl.replace trace src @@ t :: ts
    else
      Hashtbl.add trace src [t]) () t_list;
  let program = Program.{
    locs = locs;
    preds = props;
    states = states;
    final = final;
    trace = trace} in
  let pretty_json = Yojson.Safe.pretty_to_string @@ Program.to_yojson program in
  print_endline pretty_json; *)