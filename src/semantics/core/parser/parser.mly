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

%token QEXISTS
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
(*  | EXISTS; vars = separated_list(COMMA, id_target); DOT; formula = ltl_formula_target; EOF;
    { Formula.ECreate (vars, formula) @> at $sloc } *)
    | ~ = ltl_formula_target; <>

let ltl_formula_target :=
  (*| LPAREN; ~ = ltl_formula_target; RPAREN; <> *)
  | TRUE;
    { Formula.True }
  | FALSE;
    { Formula.False }
  (* | name = id_target; LPAREN; arg = id_target; RPAREN;
    { fFormula.Predicate (name, arg) @> at $sloc }
  | LNOT; formula = ltl_formula_target;
    { Formula.Not (formula) @> at $sloc }
  | f1 = ltl_formula_target; LAND; f2 = ltl_formula_target;
    { Formula.And (f1, f2) @> at $sloc }
  | f1 = ltl_formula_target; LOR; f2 = ltl_formula_target;
    { Formula.Or (f1, f2) @> at $sloc }
  | f1 = ltl_formula_target; LIMPLIES; f2 = ltl_formula_target;
    { Formula.Implication (f1, f2) @> at $sloc }
  | f1 = ltl_formula_target; LBICONDIMPL; f2 = ltl_formula_target;
    { Formula.BicondImplication (f1, f2) @> at $sloc }
  | NEXT; f = ltl_formula_target;
    { Formula.Next (f) @> at $sloc } *)
