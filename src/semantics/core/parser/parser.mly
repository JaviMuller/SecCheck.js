(* ================================ *)
(*            Definitions           *)
(* ================================ *)

%{
  open TcpgSyntax
%}


(* ========== Typed Tokens ========== *)

%token <bool>   BOOL
%token <int>    INT
%token <float>  FLOAT
%token <string> STRING
%token <string> ID

(* ========== General Symbol Tokens ========== *)

%token COMMA DOT SEMICOLON
%token LPAREN RPAREN
%token EOF

(* ========== Language Tokens ========== *)

%token PROP
%token LBRACK RBRACK
%token LBRACE RBRACE
%token EQ NEQ LT LEQ GT GEQ
%token DEFEQ

(* ========== Entry Point ========== *)

%type <Security.t> entry_sec_prop_target

%start
entry_sec_prop_target


(* ====================================== *)
(*            Grammar and Rules           *)
(* ====================================== *)

%%

let entry_sec_prop_target := ~ = sec_prop_target; EOF; <>

let sec_prop_target :=
  | PROP; name = id_target; LPAREN; vars = separated_list(COMMA, id_target); RPAREN;
    LBRACE; preds = separated_list(COMMA, predicate_target); SEMICOLON;
    trace = separated_list(COMMA, trace_target); RBRACE;
    { Security.Property (name, vars, preds, trace) }

let predicate_target :=
  | DOT; name = id_target; LPAREN; arg = id_target; RPAREN;
    { Predicate.UnaryPred (name, arg) }

let trace_target :=
  | ~ = relop_target; <>
  | func = id_target; LPAREN; args = separated_list(COMMA, expression_target); RPAREN;
    { Trace.FuncCall (func, args) }
  (* TODO: Right now it is <expr>[<expr>]=<expr>. Will it only be <var>[<expr>]=<expr>? *)
  | obj = expression_target; LBRACK; prop = expression_target; RBRACK; DEFEQ; v = expression_target;
    { Trace.PropAssign (obj, prop, v) }
  | var = id_target; DEFEQ; obj = expression_target; LBRACK; prop = expression_target; RBRACK;
    { Trace.PropLookup (var, obj, prop) }

let relop_target :=
  | e1 = expression_target; EQ; e2 = expression_target;
    { Trace.RelOp (Binrelop.Eq, e1, e2)}
  | e1 = expression_target; NEQ; e2 = expression_target;
    { Trace.RelOp (Binrelop.NEq, e1, e2)}
  | e1 = expression_target; LT; e2 = expression_target;
    { Trace.RelOp (Binrelop.Lt, e1, e2)}
  | e1 = expression_target; LEQ; e2 = expression_target;
    { Trace.RelOp (Binrelop.LEq, e1, e2)}
  | e1 = expression_target; GT; e2 = expression_target;
    { Trace.RelOp (Binrelop.Gt, e1, e2)}
  | e1 = expression_target; GEQ; e2 = expression_target;
    { Trace.RelOp (Binrelop.GEq, e1, e2)}

let expression_target :=
  | LPAREN; ~ = expression_target; RPAREN; <>
  | x = id_target;
    { Expr.Var x }
  | c = val_target;
    { Expr.Const c }

let val_target :=
  | b = BOOL;        < Value.Bool >
  | i = INT;         < Value.Int >
  | f = FLOAT;       < Value.Float >
  | s = STRING;      < Value.String >

let id_target := x = ID; { x }
