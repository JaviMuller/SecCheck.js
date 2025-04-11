(* ================================ *)
(*            Definitions           *)
(* ================================ *)

%{
  open TcpgSyntax
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
%token NAME VALUE
%token LBRACK RBRACK
%token LBRACE RBRACE
%token EQ
// %token NEQ LT LEQ GT GEQ
%token DEFEQ
%token LAND LOR LNOT

(* ========== Precedence and Associativity ========== *)

%left LOR
%left LAND
%nonassoc LNOT

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
    LBRACE; formula = formula_target; SEMICOLON;
    trace = separated_list(COMMA, trace_target); RBRACE;
    { Query.{name = name; vars = vars; formula = formula; trace = trace} }

let formula_target :=
  | LPAREN; ~ = formula_target; RPAREN; <>
  | name = name_target; LPAREN; args = separated_list(COMMA, id_target); RPAREN;
    { Formula.Predicate (name, args) }
  | NAME; LPAREN; var = id_target; RPAREN; EQ; name = STRING;
    { Formula.HasName (var, name) }
  | VALUE; LPAREN; var = id_target; RPAREN; EQ; value = val_target;
    { Formula.HasValue (var, value) }
  | LNOT; f = formula_target;
    { Formula.Not f }
  | f1 = formula_target; LAND; f2 = formula_target;
    { Formula.And [f1; f2] }
  | f1 = formula_target; LOR; f2 = formula_target;
    { Formula.Or [f1; f2] }

let trace_target :=
  // | ~ = relop_target; <>
  | func = id_target; LPAREN; args = separated_list(COMMA, id_target); RPAREN;
    { Action.FuncCall (func, args) }
  | func = id_target; LPAREN; THREEDOTS; arg = id_target; RPAREN;
    { Action.FuncCallWithArg(func, arg) }
  | func = id_target; LPAREN; THREEDOTS; RPAREN;
    { Action.FuncCallAnyArgs func }
  | obj = id_target; LBRACK; prop = id_target; RBRACK; DEFEQ; v = id_target;
    { Action.PropAssign (obj, prop, v) }
  | var = id_target; DEFEQ; obj = id_target; LBRACK; prop = id_target; RBRACK;
    { Action.PropLookup (var, obj, prop) }

// let relop_target :=
//   | e1 = id_target; EQ; e2 = id_target;
//     { Action.RelOp (Binrelop.Eq, e1, e2)}
//   | e1 = id_target; NEQ; e2 = id_target;
//     { Action.RelOp (Binrelop.NEq, e1, e2)}
//   | e1 = id_target; LT; e2 = id_target;
//     { Action.RelOp (Binrelop.Lt, e1, e2)}
//   | e1 = id_target; LEQ; e2 = id_target;
//     { Action.RelOp (Binrelop.LEq, e1, e2)}
//   | e1 = id_target; GT; e2 = id_target;
//     { Action.RelOp (Binrelop.Gt, e1, e2)}
//   | e1 = id_target; GEQ; e2 = id_target;
//     { Action.RelOp (Binrelop.GEq, e1, e2)}

let val_target :=
  | b = BOOL;        < Value.Bool >
  | i = INT;         < Value.Int >
  | f = FLOAT;       < Value.Float >
  | s = STRING;      < Value.String >

let new_id_target := x = ID; { add_variable x; x }
let id_target :=
  | UNDERSCORE; { "_" }
  | x = ID; { check_variable x; x }
let name_target := x = ID; { x }