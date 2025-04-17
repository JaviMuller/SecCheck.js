open TcpgSyntax

module VarMap = Map.Make (String)
module StateMap = Map.Make (Int)

let cartesian_product (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  List.fold_left (fun acc x -> List.fold_left (fun acc' y -> (x, y) :: acc') acc l2) [] l1

let filter_unwrap lst =
  List.filter_map (fun x -> x) lst

type state = {
  q_state : int;
  p_state : int;
  bindings : string VarMap.t;
  progress : int StateMap.t;
  formula : Formula.t;
}

let get_bindings ({ bindings; _ } : state) : string VarMap.t =
  bindings

let bindings_to_string (bindings : string VarMap.t) : string =
  VarMap.fold (fun k v acc -> Printf.sprintf "%s  %s -> %s\n" acc k v) bindings ""

let init_state (f : Formula.t) : state = {
  q_state = 0;
  p_state = 0;
  bindings = VarMap.empty;
  progress = StateMap.empty;
  formula = f;
}

let rec unify_aux (bindings : string VarMap.t option list) (vars: string list) (locs : string list list) : string VarMap.t list option =
  List.fold_left (fun (acc : string VarMap.t list option) (binding : string VarMap.t option)->
    match acc with
    | None -> None
    | Some acc' ->
      match binding with
      | None -> None
      | Some binding' ->
        match vars, locs with
        | [], [] -> Some (binding' :: acc')
        | [], _ -> None
        | _, [] -> None
        | var :: rest_vars, locs :: rest_locs ->
          (match unify_aux bindings rest_vars rest_locs with
          | None -> None
          | Some bindings_rest ->
            (match VarMap.find_opt var binding' with
            | Some l ->
              if List.exists (fun x -> x = l) locs then
                Some (bindings_rest @ acc')
              else
                Some acc'
            | None ->
              let new_binds = List.concat @@ List.map (fun bind -> 
                                                       List.map (fun add_to_map -> add_to_map bind) @@
                                                                List.map (VarMap.add var) locs) bindings_rest in
              let new_acc = new_binds @ acc' in
              Some new_acc))
            (* For each of the bindings that resulted from the recursive call, generate 
               a list of maps, one for each mapping x->v for v in locs _rest*)
  ) (Some []) bindings

let unify (bindings : string VarMap.t) (q_action : Qaction.t) (p_action : Paction.t) : string VarMap.t list option =
  match q_action, p_action with
  | FuncCall (nameq, argsq), FuncCall (namep, argsp) ->
    unify_aux [Some bindings] (nameq :: argsq) (namep :: argsp)
  | FuncCallWithArg (nameq, argq), FuncCall(namep, argsp) ->
    let locs_all_args = List.sort_uniq compare @@ List.concat argsp in
    unify_aux [Some bindings] [nameq; argq] [namep; locs_all_args]
  | FuncCallAnyArgs nameq, FuncCall(namep, _) ->
    unify_aux [Some bindings] [nameq] [namep]
  | PropAssign (objq, propq, vq), PropAssign (objp, propp, vp) ->
    unify_aux [Some bindings] [objq; propq; vq] [objp; propp; vp]
  | PropLookup (varq, objq, propq), PropLookup (varp, objp, propp) ->
    unify_aux [Some bindings] [varq; objq; propq] [varp; objp; propp]
  | _, _ -> None

let rec is_sat (bindings : string VarMap.t) (p : Program.t) (formula : Formula.t) : Formula.t =
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
    if List.exists (fun f' -> f' = Formula.False) fs' then
      False
    else if List.for_all (fun f' -> f' = Formula.True) fs' then
      True
    else
      And (List.filter (fun f' -> f' <> Formula.True) fs')
  | Or fs ->
    let fs' = List.map (is_sat bindings p) fs in
    if List.exists (fun f' -> f' = Formula.True) fs' then
      True
    else if List.for_all (fun f' -> f' = Formula.False) fs' then
      False
    else
      Or (List.filter (fun f' -> f' <> Formula.False) fs')

let analyze_transition (s : state) (p : Program.t) (q_action : Qaction.t) (t : Transition.t) : state list =
  let new_p_state = Transition.get_dest t in
  let p_action = Transition.get_action t in
  match unify s.bindings q_action p_action with
  | None ->
    let new_progress = StateMap.add new_p_state s.q_state s.progress in
    (match StateMap.find_opt new_p_state s.progress with
    | Some q_s ->
      if q_s = s.q_state then
        (* Same code state, no query progress *)
        []
      else
        [{ s with p_state = new_p_state; progress = new_progress }]
    | None ->
      [{ s with p_state = new_p_state; progress = new_progress }])
  | Some new_bindings ->
    let new_q_state = s.q_state + 1 in
    let new_progress = StateMap.add new_p_state new_q_state s.progress in
    filter_unwrap @@ List.map (fun new_b ->
        let new_formula = is_sat new_b p s.formula in  
        (match new_formula with
        | False -> None
        | _ ->
          let new_q_state = s.q_state + 1 in
          Some { q_state = new_q_state;
              p_state = new_p_state;
              bindings = new_b;
              progress = new_progress;
              formula = new_formula; })) new_bindings

let transition (s : state) (q : Query.t) (p : Program.t) : state list =
  let p_transitions = Program.state_transitions p s.p_state in
  let q_action = Query.get_state_action q s.q_state in
  List.concat @@ List.map (analyze_transition s p q_action) p_transitions

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
  aux [init_state @@ Query.get_formula q] []