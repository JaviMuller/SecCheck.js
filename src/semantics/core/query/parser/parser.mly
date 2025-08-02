(* ================================ *)
(*            Definitions           *)
(* ================================ *)

%{
  open SecCheckJSSyntax
  module Future = Trcformula.Future
  module Past = Trcformula.Past
  let variables = Hashtbl.create 10
  let add_variable name = Hashtbl.add variables name true
  let check_variable name = if not (Hashtbl.mem variables name) then
    failwith ("Error: Variable " ^ name ^ " is unbound.")
%}


(* ========== Typed Tokens ========== *)

%token <bool>   BOOL
%token <int>    INT
%token <float>  FLOAT
%token <string> STRING
%token <string> ID

(* ========== General Symbol Tokens ========== *)

%token COMMA SEMICOLON
%token UNDERSCORE THREEDOTS
%token LPAREN RPAREN
%token EOF

(* ========== Language Tokens ========== *)

%token PROP
%token LBRACK RBRACK
%token LBRACE RBRACE
%token DEFEQ

(* ========== Predicate Names ========== *)

%token BASE_TAINT
%token SINK SANITIZER HAS_VALUE
%token HAS_FUN_NAME
%token DEPENDS TAINTED SAME_VALUE

(* ========== Logic Tokens ======== *)
%token TRUE FALSE
%token LNOT LAND LOR
%token LIMPL LEQUIV
%token NEXT WEAKNEXT
%token UNTIL RELEASE
%token EVENTUALLY ALWAYS
%token PAST
%token BEFORE WEAKBEFORE
%token SINCE PASTRELEASE
%token ONCE HISTORICALLY

(* ========== Precedence and Associativity ========== *)

%right UNTIL RELEASE
%right SINCE PASTRELEASE
%right COMMA
%right LEQUIV
%right LIMPL
%left LOR
%left LAND
%nonassoc LNOT
%nonassoc NEXT WEAKNEXT EVENTUALLY ALWAYS
%nonassoc BEFORE WEAKBEFORE ONCE HISTORICALLY 

(* ========== Entry Point ========== *)

%type <Query.t> entry_sec_prop_target

%start
entry_sec_prop_target


(* ====================================== *)
(*            Grammar and Rules           *)
(* ====================================== *)

%%

let entry_sec_prop_target := ~ = sec_prop_target; EOF; <>

let sec_prop_target :=
  | PROP; name = name_target; LPAREN; vars = separated_list(COMMA, new_id_target); RPAREN;
    LBRACE; invariant = invariant_target; SEMICOLON;
    tracef = trace_formula_target; RBRACE;
    { Query.{name = name; vars = vars; invariant = invariant; tracef = tracef} }

let invariant_target :=
  | LPAREN; ~ = invariant_target; RPAREN; <>
  | TRUE;
    { Invformula.True }
  | FALSE;
    { Invformula.False }
  | p = pred_target;
    { Invformula.Pred p }
  | LNOT; f = invariant_target;
    { Invformula.Not f }
  | f1 = invariant_target; LAND; f2 = invariant_target;
    { Invformula.And [f1; f2] }
  | f1 = invariant_target; LOR; f2 = invariant_target;
    { Invformula.Or [f1; f2] }

let pred_target :=
  | BASE_TAINT; LPAREN; v = id_target; RPAREN;
    { Qpred.BaseTaint v }
  | SINK; LPAREN; v = id_target; RPAREN;
    { Qpred.Sink v }
  | SANITIZER; LPAREN; v = id_target; RPAREN;
    { Qpred.Sanitizer v }
  | HAS_VALUE; LPAREN; x = id_target; COMMA; v = val_target; RPAREN;
    { Qpred.HasValue (x, v) }
  | HAS_FUN_NAME; LPAREN; f = id_target; COMMA; name = name_target; RPAREN;
    { Qpred.HasFunName (f, name) }
  | DEPENDS; LPAREN; v1 = id_target; COMMA; v2 = id_target; RPAREN;
    { Qpred.Depends (v1, v2) }
  | TAINTED; LPAREN; v1 = id_target; RPAREN;
    { Qpred.Tainted (v1) }
  | SAME_VALUE; LPAREN; v1 = id_target; COMMA; v2 = id_target; RPAREN;
    { Qpred.SameValue (v1, v2) }

let trace_formula_target :=
  | f = future_formula_target;
    { Trcformula.FutureFormula f }
  | LPAREN; PAST; RPAREN; f = past_formula_target;
    { Trcformula.PastFormula f }

let future_formula_target :=
  | TRUE;
    { Future.True }
  | FALSE;
    { Future.False }
  | LPAREN; ~ = future_formula_target; RPAREN; <>
  | qaction = query_action_target;
    { Future.Action qaction }
  | LNOT; f = future_formula_target;
    { Future.Not f }
  | f1 = future_formula_target; COMMA; f2 = future_formula_target;
    { Future.Sequence [f1; f2] }
  | f1 = future_formula_target; LAND; f2 = future_formula_target;
    { Future.And [f1; f2] }
  | f1 = future_formula_target; LOR; f2 = future_formula_target;
    { Future.Or [f1; f2]}
  | f1 = future_formula_target; LIMPL; f2 = future_formula_target;
    { Future.Implication (f1, f2) }
  | f1 = future_formula_target; LEQUIV; f2 = future_formula_target;
    { Future.Equivalence (f1, f2) }
  | NEXT; f = future_formula_target;
    { Future.Next f }
  | WEAKNEXT; f = future_formula_target;
    { Future.WeakNext f }
  | f1 = future_formula_target; UNTIL; f2 = future_formula_target;
    { Future.Until (f1, f2) }
  | f1 = future_formula_target; RELEASE; f2 = future_formula_target;
    { Future.Release (f1, f2) }
  | EVENTUALLY; f = future_formula_target;
    { Future.Eventually f }
  | ALWAYS; f = future_formula_target;
    { Future.Always f }

let past_formula_target :=
  | TRUE;
    { Past.True }
  | FALSE;
    { Past.False }
  | LPAREN; ~ = past_formula_target; RPAREN; <>
  | qaction = query_action_target;
    { Past.Action qaction }
  | LNOT; f = past_formula_target;
    { Past.Not f }
  | f1 = past_formula_target; LAND; f2 = past_formula_target;
    { Past.And [f1; f2] }
  | f1 = past_formula_target; LOR; f2 = past_formula_target;
    { Past.Or [f1; f2] }
  | f1 = past_formula_target; LIMPL; f2 = past_formula_target;
    { Past.Implication (f1, f2) }
  | f1 = past_formula_target; LEQUIV; f2 = past_formula_target;
    { Past.Equivalence (f1, f2) }
  | BEFORE; f = past_formula_target;
    { Past.Before f }
  | WEAKBEFORE; f = past_formula_target;
    { Past.WeakBefore f }
  | f1 = past_formula_target; SINCE; f2 = past_formula_target;
    { Past.Since (f1, f2) }
  | f1 = past_formula_target; PASTRELEASE; f2 = past_formula_target;
    { Past.PastRelease (f1, f2) }
  | ONCE; f = past_formula_target;
    { Past.Once f }
  | HISTORICALLY; f = past_formula_target;
    { Past.Historically f }

let query_action_target :=
  // | ~ = relop_target; <>
  | func = id_target; LPAREN; args = separated_list(COMMA, id_target); RPAREN;
    { Qaction.FuncCall (func, args) }
  | func = id_target; LPAREN; THREEDOTS; arg = id_target; RPAREN;
    { Qaction.FuncCallWithArg(func, arg) }
  | func = id_target; LPAREN; THREEDOTS; RPAREN;
    { Qaction.FuncCallAnyArgs func }
  | obj = id_target; LBRACK; prop = id_target; RBRACK; DEFEQ; v = id_target;
    { Qaction.PropAssign (obj, prop, v) }
  | var = id_target; DEFEQ; obj = id_target; LBRACK; prop = id_target; RBRACK;
    { Qaction.PropLookup (var, obj, prop) }

let val_target :=
  | b = BOOL;        < Value.Bool >
  | i = INT;         < Value.Int >
  | f = FLOAT;       < Value.Float >
  | s = STRING;      < Value.String >

let new_id_target := x = ID; { add_variable x; x }
let id_target :=
  | UNDERSCORE; { Qvar.UVar }
  | x = ID; { check_variable x; Qvar.Var x }
let name_target := x = ID; { x }