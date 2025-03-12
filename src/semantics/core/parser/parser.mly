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

%token COMMA DOT COLON
%token LPAREN RPAREN
%token EOF

(* ========== Language Tokens ========== *)

%token LBRACK RBRACK
%token EQ NEQ LT LEQ GT GEQ
%token DEFEQ
%token TAINTED

(* ========== Classical Logic Tokens ========== *)

%token TRUE
%token FALSE
%token QEXISTS QFORALL
%token LNOT
%token LAND LOR
%token LIMPLIES LEQUIV

(* ========== Temporal Logic Tokens ========== *)

%token NEXT EVENTUALLY ALWAYS
%token UNTIL WEAKUNTIL

(* ========== Precedence and Associativity ========== *)

%nonassoc QEXISTS QFORALL
%right LEQUIV
%right LIMPLIES
%left LOR
%left LAND
%left UNTIL WEAKUNTIL
%nonassoc LNOT NEXT EVENTUALLY ALWAYS

(* ========== Entry Point ========== *)

%type <Formula.t> entry_formula_target

%start
entry_formula_target



(* ====================================== *)
(*            Grammar and Rules           *)
(* ====================================== *)

%%

let entry_formula_target := ~ = formula_target; EOF; <>

let formula_target :=
  | LPAREN; ~ = formula_target; RPAREN; <>
  | QEXISTS; vars = separated_list(COMMA, id_target); COLON; f = formula_target;
    { Formula.Exists (vars, f) } %prec QEXISTS
  | QFORALL; vars = separated_list(COMMA, id_target); COLON; f = formula_target;
    { Formula.Forall (vars, f) } %prec QFORALL
  | f1 = formula_target; UNTIL; f2 = formula_target;
    { Formula.Until (f1, f2) }
  | f1 = formula_target; WEAKUNTIL; f2 = formula_target;
    { Formula.WeakUntil (f1, f2) }
  | f1 = formula_target; LAND; f2 = formula_target;
    { Formula.And [f1; f2] }
  | f1 = formula_target; LOR; f2 = formula_target;
    { Formula.Or [f1; f2] }
  | f1 = formula_target; LIMPLIES; f2 = formula_target;
    { Formula.Implication (f1, f2) }
  | f1 = formula_target; LEQUIV; f2 = formula_target;
    { Formula.Equiv (f1, f2) }
  | LNOT; f = formula_target;
    { Formula.Not f }
  | NEXT; f = formula_target;
    { Formula.Next f }
  | EVENTUALLY; f = formula_target;
    { Formula.Eventually f }
  | ALWAYS; f = formula_target;
    { Formula.Always f }
  | TRUE;
    { Formula.True }
  | FALSE;
    { Formula.False }
  | ~ = formula_relop_target; <>
  | DOT; TAINTED; LPAREN; e = expression_target; RPAREN;
    { Formula.Tainted e }
  | DOT; pred = id_target; LPAREN; expr = expression_target; RPAREN;
    { Formula.PredAssertion (pred, expr) }
  | e1 = expression_target; LBRACK; e2 = expression_target; RBRACK; DEFEQ; e3 = expression_target;
    { Formula.PropAssign (e1, e2, e3) }
  | e1 = expression_target; DEFEQ; e2 = expression_target; LBRACK; e3 = expression_target; RBRACK;
    { Formula.PropLookUp (e1, e2, e3) }
  | func = expression_target; LPAREN; args = separated_list(COMMA, expression_target); RPAREN;
    { Formula.Call (func, args) }

let formula_relop_target :=
  | e1 = expression_target; EQ; e2 = expression_target;
    { Formula.RelOp (Binrelop.Eq, e1, e2)}
  | e1 = expression_target; NEQ; e2 = expression_target;
    { Formula.RelOp (Binrelop.NEq, e1, e2)}
  | e1 = expression_target; LT; e2 = expression_target;
    { Formula.RelOp (Binrelop.Lt, e1, e2)}
  | e1 = expression_target; LEQ; e2 = expression_target;
    { Formula.RelOp (Binrelop.LEq, e1, e2)}
  | e1 = expression_target; GT; e2 = expression_target;
    { Formula.RelOp (Binrelop.Gt, e1, e2)}
  | e1 = expression_target; GEQ; e2 = expression_target;
    { Formula.RelOp (Binrelop.GEq, e1, e2)}

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
