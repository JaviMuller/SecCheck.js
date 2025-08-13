open SecCheckJSSyntax

module VarMap = Map.Make (struct
  type t = Qvar.t
  let compare = compare
end)

module ProgressSet = Set.Make (struct
  type t = int * int
  let compare = compare 
end)

let cartesian_product_list (lists : 'a list list) : 'a list list =
  let combine acc lst =
    List.concat (List.map (fun acc_elem ->
      List.map (fun elem -> elem :: acc_elem) lst) acc)
  in
  List.fold_left combine [[]] lists
  |> List.map List.rev

let cartesian_product (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  List.concat @@ List.map (fun e1 -> List.map (fun e2 -> (e1, e2)) l2) l1

let filter_unwrap lst =
  List.filter_map (fun x -> x) lst

type state = {
  q_state   : int;
  p_state   : int;
  bindings  : Ploc.t VarMap.t;
  progress  : ProgressSet.t;
  invariant : Invformula.t;
  p_preds   : Pautomaton.pred; (* Some program predicates are concretized during search *)
  q_trans   : Qtransition.t list;
  p_trans   : Ptransition.t list;
}

let get_bindings ({ bindings; _ } : state) : Ploc.t VarMap.t =
  bindings

let bindings_to_string (bindings : Ploc.t VarMap.t) : string =
  VarMap.fold (fun k v acc -> Printf.sprintf "%s  %s -> %s\n" acc (Qvar.to_string k) @@ Ploc.to_string v) bindings ""

let init_state (q : Qautomaton.t) (p : Pautomaton.t) : state = {
  q_state = 1;
  p_state = 1;
  bindings = VarMap.empty;
  progress = ProgressSet.empty;
  invariant = q.invariant;
  p_preds = p.preds;
  q_trans = [];
  p_trans = []
}

let unify_aux (bindings : Ploc.t VarMap.t) (vars: Qvar.t list) (locs : Ploc.t list list) : Ploc.t VarMap.t list =
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
      []
    else
      let locs' = cartesian_product_list filtered_locs in
      filter_unwrap @@ List.map (fun ls ->
        let kv_pairs = List.combine vars ls in
        List.fold_left (fun opt_new_binds (key, value) ->
          match opt_new_binds with
          | None -> None
          | Some map ->
            (match key, VarMap.find_opt key map with
             | Var _, None -> Some (VarMap.add key value map)
             | Var _, Some v' ->
               if v' = value then
                 Some map
               else
                 None
             (* For UVar, do not add a binding *)
             | UVar, _ -> Some map
            )) 
          (Some bindings) kv_pairs
        ) locs'
  with
  | Invalid_argument _ -> []


let unify (bindings : Ploc.t VarMap.t) (q_action : Qaction.t) (p_action : Paction.t) : Ploc.t VarMap.t list =
  match q_action, p_action with
  | FuncCall (nameq, argsq), FuncCall (namep, argsp) ->
    unify_aux bindings (nameq :: argsq) (namep :: argsp)
  | FuncCallWithArg (nameq, argq), FuncCall(namep, argsp) ->
    let locs_all_args = List.sort_uniq compare @@ List.concat argsp in
    unify_aux bindings [nameq; argq] [namep; locs_all_args]
  | FuncCallAnyArgs nameq, FuncCall(namep, _) ->
    unify_aux bindings [nameq] [namep]
  | FuncRet (calleeq, retq), FuncRet (calleep, retp) ->
    unify_aux bindings [calleeq; retq] [calleep; retp]
  | PropAssign (objq, propq, vq), PropAssign (objp, propp, vp) ->
    unify_aux bindings [objq; propq; vq] [objp; propp; vp]
  | PropLookup (varq, objq, propq), PropLookup (varp, objp, propp) ->
    unify_aux bindings [varq; objq; propq] [varp; objp; propp]
  | Any, _ ->
    [bindings]
  | _, _ -> []

let rec is_sat (bindings : Ploc.t VarMap.t) (formula : Invformula.t) (facts : Pautomaton.pred) : (Invformula.t * Pautomaton.pred) =
  match formula with
  | True -> True, facts
  | False -> False, facts
  | Pred p ->
    (match p with
     | BaseTaint v ->
       (match VarMap.find_opt v bindings with
        | Some loc ->
          if Pautomaton.is_base_taint facts loc
            then True, facts
          else False, facts
        | None -> Pred p, facts)
     | Sink v ->
       (match VarMap.find_opt v bindings with
        | Some loc ->
          if Pautomaton.is_sink facts loc
            then True, facts
        else False, facts
        | None -> Pred p, facts)
     | Sanitizer v ->
       (match VarMap.find_opt v bindings with
        | Some loc ->
          if Pautomaton.is_sanitizer facts loc
            then True, facts
          else False, facts
        | None -> Pred p, facts)
     | HasValue (x, v) ->
       (match VarMap.find_opt x bindings with
        | Some loc ->
          (match Pautomaton.has_value facts loc v with
           | Some new_facts ->
             Pred p, new_facts
           | None -> False, facts)
        | None -> Pred p, facts)
     | HasFunName (v, name) ->
       (match VarMap.find_opt v bindings with
        | Some loc ->
          if Pautomaton.has_fun_name facts loc name
            then True, facts
          else False, facts
        | None -> Pred p, facts)
     | Depends (v1, v2) ->
       (match VarMap.find_opt v1 bindings, VarMap.find_opt v2 bindings with
        | Some loc1, Some loc2 ->
          if Pautomaton.depends facts loc1 loc2
            then True, facts
          else False, facts
        | None, _ | _, None -> Pred p, facts)
     | Tainted v ->
       (match VarMap.find_opt v bindings with
        | Some loc ->
          if Pautomaton.is_tainted facts loc
            then True, facts
          else False, facts
        | None -> Pred p, facts)
     | SameValue (v1, v2) ->
       (match VarMap.find_opt v1 bindings, VarMap.find_opt v2 bindings with
        | Some loc1, Some loc2 ->
          (match Pautomaton.same_value facts loc1 loc2 with
           | Some new_facts -> Pred p, new_facts
           | None -> False, facts)
        | None, _ | _, None -> Pred p, facts))
  | Not f ->
    (match is_sat bindings f facts with
    | True, new_facts -> False, new_facts
    | False, new_facts -> True, new_facts
    | f', new_facts -> Not f', new_facts)
  | And fs ->
    let fs', new_facts = List.fold_left (fun (fs, facts) f ->
      let f', facts' = is_sat bindings f facts in
      (f' :: fs, facts')) ([], facts) fs in
    if List.exists (fun f' -> f' = Invformula.False) fs' then
      False, new_facts
    else if List.for_all (fun f' -> f' = Invformula.True) fs' then
      True, new_facts
    else
      And (List.filter (fun f' -> f' <> Invformula.True) fs'), new_facts
  | Or fs ->
    let fs', new_facts = List.fold_left (fun (fs, facts) f ->
      let f', facts' = is_sat bindings f facts in
      (f' :: fs, facts')) ([], facts) fs in
    if List.exists (fun f' -> f' = Invformula.True) fs' then
      True, new_facts
    else if List.for_all (fun f' -> f' = Invformula.False) fs' then
      False, new_facts
    else
      Or (List.filter (fun f' -> f' <> Invformula.False) fs'), new_facts

(* analyze_transition : is_progress -> can_bind -> bind_is_sat *)
let analyze_transition (s : state) (q_trans : Qtransition.t) (p_trans : Ptransition.t) : state list =
  let new_q_state = Qtransition.get_dest q_trans in
  let new_p_state = Ptransition.get_dest p_trans in
  let q_action = Qtransition.get_lbl q_trans in
  let p_action = Ptransition.get_lbl p_trans in
  (* is_progress*)
  if ProgressSet.mem (new_q_state, new_p_state) s.progress
    then []
    else
      let new_progress = ProgressSet.add (new_q_state, new_p_state) s.progress in
      (* can_bind *)
      let new_bindings = unify s.bindings q_action p_action in
      filter_unwrap @@ List.map (fun new_b ->
        (* is_sat *)
        (match is_sat new_b s.invariant s.p_preds with
         | False, _ -> None
         | new_inv, new_preds ->
           Some {
             q_state = new_q_state;
             p_state = new_p_state;
             bindings = new_b;
             progress = new_progress;
             invariant = new_inv;
             p_preds = new_preds;
             q_trans = q_trans :: s.q_trans;
             p_trans = p_trans :: s.p_trans; })) new_bindings

let transition (s : state) (q : Qautomaton.t) (p : Pautomaton.t) : state list =
  let q_transitions = Qautomaton.state_transitions q s.q_state in
  let p_transitions = Pautomaton.state_transitions p s.p_state in
  List.concat @@ List.map (fun (q_trans, p_trans) -> analyze_transition s q_trans p_trans) @@ cartesian_product q_transitions p_transitions

let verify (q : Qautomaton.t) (p : Pautomaton.t) : state list =
  let rec aux (i_states : state list) (o_states : state list) : state list =
    match i_states with
    | [] -> o_states
    | s :: rest ->
      if Qautomaton.is_acc_state q s.q_state
         &&
         Pautomaton.is_acc_state p s.p_state then
        aux rest (s :: o_states)
      else
        let new_states = transition s q p in
        (* BFS *)
        aux (rest @ new_states) o_states in
  aux [init_state q p] []