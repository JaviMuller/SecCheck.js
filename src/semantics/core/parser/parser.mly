(* ================================ *)
(*            Definitions           *)
(* ================================ *)

%{
  open TcpgSyntax
%}


(* ========== Typed Tokens ========== *)

%token <string> ID

(* ========== Language Tokens ========== *)

%token TRUE
%token FALSE

(* ========== Symbol Tokens ========== *)

%token COMMA DOT
%token LPAREN RPAREN
%token LBRACK RBRACK
%token DEFEQ
%token EOF

(* ========== Classical Logic Tokens ========== *)

%token QEXISTS QFORALL
%token LNOT
%token LAND LOR
%token LIMPLIES LBICONDIMPL

(* ========== Temporal Logic Tokens ========== *)

%token NEXT EVENTUALLY ALWAYS
%token UNTIL WEAKUNTIL

(* ========== Precedence and Associativity ========== *)

%left LAND LOR
%right LIMPLIES LBICOND

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
  | QEXISTS; vars = separated_list(COMMA, id_target); DOT; f = formula_target;
    { Formula.Exists (vars, f) }
  | QFORALL; vars = separated_list(COMMA, id_target); DOT; f = formula_target;
    { Formula.Forall (vars, f) }
  // | ~ = ltl_formula_target; <> -- When passing to ltl, we only need to consider prenex existential formulas

// let ltl_formula_target :=
  | LPAREN; ~ = formula_target; RPAREN; <>
  | TRUE;
    { Formula.True }
  | FALSE;
    { Formula.False }
  // | name = id_target; LPAREN; arg = id_target; RPAREN;
  //   { Formula.Predicate (name, arg) }
  | name = id_target;
    { Formula.Variable (name) }
  | LNOT; f = formula_target;
    { Formula.Not (f) }
  | f1 = formula_target; LAND; f2 = formula_target;
    { Formula.And (f1, f2) }
  | f1 = formula_target; LOR; f2 = formula_target;
    { Formula.Or (f1, f2) }
  | f1 = formula_target; LIMPLIES; f2 = formula_target;
    { Formula.Implication (f1, f2) }
  | f1 = formula_target; LBICONDIMPL; f2 = formula_target;
    { Formula.BicondImplication (f1, f2) }
  
let id_target := x = ID; { x }
