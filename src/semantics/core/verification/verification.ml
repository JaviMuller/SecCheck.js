open TcpgSyntax

module VarMap = Map.Make (String)
module StateMap = Map.Make (Int)

let cartesian_product (lists : 'a list list) : 'a list list =
  let combine acc lst =
    List.concat (List.map (fun acc_elem ->
      List.map (fun elem -> elem :: acc_elem) lst) acc)
  in
  List.fold_left combine [[]] lists
  |> List.map List.rev

let filter_unwrap lst =
  List.filter_map (fun x -> x) lst

type state = {
  q_state   : int;
  p_state   : int;
  bindings  : string VarMap.t;
  progress  : int StateMap.t;
  invariant : Invformula.t;
}

let get_bindings ({ bindings; _ } : state) : string VarMap.t =
  bindings

let bindings_to_string (bindings : string VarMap.t) : string =
  VarMap.fold (fun k v acc -> Printf.sprintf "%s  %s -> %s\n" acc k v) bindings ""

let init_state (f : Invformula.t) : state = {
  q_state = 0;
  p_state = 0;
  bindings = VarMap.empty;
  progress = StateMap.empty;
  invariant = f;
}

let unify_aux (bindings : string VarMap.t) (vars: string list) (locs : string list list) : string VarMap.t list option =
  try
    (* Preprocessing for efficiency (will apply constraints found in bindings before any of the new applications) *)
    let var_locs = List.combine vars locs in
    let filtered_locs = List.map (fun (var, ls) ->
      match VarMap.find_opt var bindings with
      | None -> ls
      | Some l ->
        if List.exists (fun x -> x = l) ls then
          [l]
        else
          []) var_locs in
    if List.exists (fun l -> l = []) filtered_locs then
      None
    else
      let locs' = cartesian_product filtered_locs in
      Some (filter_unwrap @@ List.map (fun ls ->
        let kv_pairs = List.combine vars ls in
        List.fold_left (fun o_map (key, value) ->
          match o_map with
          | None -> None
          | Some map ->
            (match VarMap.find_opt key map with
            | None -> Some (VarMap.add key value map)
            | Some v' ->
              if v' = value then
                Some map
              else
                None
            )) 
          (Some bindings) kv_pairs
        ) locs')
  with
  | Invalid_argument _ -> None


let unify (bindings : string VarMap.t) (q_action : Qaction.t) (p_action : Paction.t) : string VarMap.t list option =
  match q_action, p_action with
  | FuncCall (nameq, argsq), FuncCall (namep, argsp) ->
    unify_aux bindings (nameq :: argsq) (namep :: argsp)
  | FuncCallWithArg (nameq, argq), FuncCall(namep, argsp) ->
    let locs_all_args = List.sort_uniq compare @@ List.concat argsp in
    unify_aux bindings [nameq; argq] [namep; locs_all_args]
  | FuncCallAnyArgs nameq, FuncCall(namep, _) ->
    unify_aux bindings [nameq] [namep]
  | PropAssign (objq, propq, vq), PropAssign (objp, propp, vp) ->
    unify_aux bindings [objq; propq; vq] [objp; propp; vp]
  | PropLookup (varq, objq, propq), PropLookup (varp, objp, propp) ->
    unify_aux bindings [varq; objq; propq] [varp; objp; propp]
  | _, _ -> None

let rec is_sat (bindings : string VarMap.t) (p : Program.t) (formula : Invformula.t) : Invformula.t =
  match formula with
  | True -> True
  | False -> False
  | Predicate (name, args) ->
    let args' = List.map (fun arg ->
        (match arg with
        | Expr.Var v ->
          (match VarMap.find_opt v bindings with
           | Some v' -> Expr.Loc v'
           | None -> arg)
        | _ -> arg)) args in
    let is_concrete = List.for_all Expr.is_concrete args' in
    if is_concrete then
      let args'' = filter_unwrap @@ List.map Expr.to_conc args' in
      if Program.has_pred p name args'' then
        True
      else
        False
    else
      Predicate (name, args')
  | Not f ->
    (match is_sat bindings p f with
    | True -> False
    | False -> True
    | f' -> Not f')
  | And fs ->
    let fs' = List.map (is_sat bindings p) fs in
    if List.exists (fun f' -> f' = Invformula.False) fs' then
      False
    else if List.for_all (fun f' -> f' = Invformula.True) fs' then
      True
    else
      And (List.filter (fun f' -> f' <> Invformula.True) fs')
  | Or fs ->
    let fs' = List.map (is_sat bindings p) fs in
    if List.exists (fun f' -> f' = Invformula.True) fs' then
      True
    else if List.for_all (fun f' -> f' = Invformula.False) fs' then
      False
    else
      Or (List.filter (fun f' -> f' <> Invformula.False) fs')

let analyze_transition (s : state) (q : Query.t) (p : Program.t) (q_action : Qaction.t) (t : Transition.t) : state list =
  let new_p_state = Transition.get_dest t in
  let p_action = Transition.get_action t in
  let q_next_state = Query.next_state q s.q_state in
  let p_new_progress = StateMap.add new_p_state s.q_state s.progress in
  let p_q_new_progress = StateMap.add new_p_state q_next_state s.progress in
  let p_new_state = { s with p_state = new_p_state; progress = p_new_progress } in
  match unify s.bindings q_action p_action with
  | None ->
    (match StateMap.find_opt new_p_state s.progress with
    | Some q_s ->
      if q_s = s.q_state then
        (* Same code state, no query progress *)
        []
      else
        [p_new_state]
    | None ->
      [p_new_state])
  | Some new_bindings ->
    (* No query transition + all possible transitions given different bindings *)
    let q_progress_new_states = filter_unwrap @@ List.map (fun new_b ->
        let new_invariant = is_sat new_b p s.invariant in  
        (match new_invariant with
        | False -> None
        | _ ->
          Some { q_state = q_next_state;
              p_state = new_p_state;
              bindings = new_b;
              progress = p_q_new_progress;
              invariant = new_invariant; })) new_bindings in
    match StateMap.find_opt new_p_state s.progress with
    | Some q_s ->
      if q_s = s.q_state then
        q_progress_new_states
      else
        p_new_state :: q_progress_new_states
    | None ->
      p_new_state :: q_progress_new_states

let transition (s : state) (q : Query.t) (p : Program.t) : state list =
  let p_transitions = Program.state_transitions p s.p_state in
  let q_action = Query.get_state_action q s.q_state in
  List.concat @@ List.map (analyze_transition s q p q_action) p_transitions

let verify (q : Query.t) (p : Program.t) : state list =
  let rec aux (i_states : state list) (o_states : state list) : state list =
    match i_states with
    | [] -> o_states
    | s :: rest ->
      if Query.is_final q s.q_state then
        aux rest (s :: o_states)
      else if Program.is_final p s.p_state then
        aux rest o_states
      else
        let new_states = transition s q p in
        (* BFS *)
        aux (rest @ new_states) o_states in
  aux [init_state @@ Query.get_invariant q] []