
%{ (* header - OCaml declarations *)
	open Printf
	open Tcore_types

	let info_error n =
		let start_pos = Parsing.rhs_start_pos n in
		let end_pos = Parsing.rhs_end_pos n in
		printf "%d.%d-%d.%d: Syntax error\n"
		start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
		end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol)
%}
	/* Ocamlyacc declarations */

%token EOF

%token LPAREN RPAREN
%token LCURLY_BRACKET RCURLY_BRACKET
%token LSQUARE_BRACKET RSQUARE_BRACKET

%token SEMICOLON
%token LET LETREC IN
%token CASE OF ALT_ARROW IF THEN ELSE
%token LAMBDA LAMBDA_DOT
%token COMMA UNDERSCORE
%token HASH CARET APOSTROPHE
%token ASSIGNMENT
%token TYPE

%token TRUE FALSE
%token <int> NUMBER
%token <string> CAPITALNAME
%token <string> VARIABLE
%token OR NOT AND LT GT LE GE EQ NE
%token PLUS MINUS MULTIPLY DIVIDE UNARY_MINUS NEG

%left OR
%left AND
%left NOT
%left LT GT LE GE
%left EQ NE
%left PLUS MINUS
%left MULTIPLY DIVIDE
%left UNARY_MINUS NEG

%start program
%type <Tcore_types.tcoreProgram> program

%% /* grammar rules */
program: topdef SEMICOLON program { $1 :: $3 }
	| topdef EOF { [$1] }
;

topdef: supcomb { Supercombinator($1) }
	| TYPE typedef { TypeDefinition($2) }
;

typedef: tintr ASSIGNMENT adts { ($1, $3) }
;

tintr: CAPITALNAME varsz { ($1, $2) }
;

adts: adt OR adts { $1 :: $3 }
	| adt { [$1] }
;

adt: CAPITALNAME adtargsz { ($1, $2) }
;

adtargsz: /* empty */ { [] }
	| adtarg adtargsz { $1 :: $2 }
;

adtarg: CAPITALNAME { Type(($1, [])) }
	| LPAREN tintr RPAREN { Type($2) }
	| VARIABLE { TVar($1) }
;

supcomb: VARIABLE varsz ASSIGNMENT expr
	{ ($1, $2, $4) }
;

expr: expr aexpr { EAppl($1, $2) }
	| expr PLUS expr { EAppl(EAppl(EVar "+", $1), $3) }
	| expr MINUS expr { EAppl(EAppl(EVar "-", $1), $3) }
	| expr MULTIPLY expr { EAppl(EAppl(EVar "*", $1), $3) }
	| expr DIVIDE expr { EAppl(EAppl(EVar "/", $1), $3) }
	| expr OR expr { EAppl(EAppl(EVar "|", $1), $3) }
	| expr AND expr { EAppl(EAppl(EVar "&", $1), $3) }
	| expr LT expr { EAppl(EAppl(EVar "<", $1), $3) }
	| expr LE expr { EAppl(EAppl(EVar "<=", $1), $3) }
	| expr EQ expr { EAppl(EAppl(EVar "==", $1), $3) }
	| expr NE expr { EAppl(EAppl(EVar "!=", $1), $3) }
	| expr GE expr { EAppl(EAppl(EVar ">=", $1), $3) }
	| expr GT expr { EAppl(EAppl(EVar ">", $1), $3) }
	| TRUE { EVar "true" }
	| FALSE { EVar "false" }
/*	| CAPITALNAME { EConstr($1) } */
	| NEG aexpr { EAppl(EVar "neg", $2) }
	| NOT aexpr { EAppl(EVar "not", $2) }
	| IF expr THEN expr ELSE expr { 
		EAppl(EAppl(EAppl(EVar "if", $2), $4), $6) }
/*	| MINUS expr %prec UNARY_MINUS { EAppl(EAppl(EVar "-", ENum 0), $2) } */
	| LET defns IN expr { ELet(Tcore_types.nonRecursive, $2, $4) }
	| LETREC defns IN expr { ELet(Tcore_types.recursive, $2, $4) }
	| CASE expr OF alts { ECase($2, $4) }
	| LAMBDA vars LAMBDA_DOT expr { ELambd($2, $4) }
	| aexpr { $1 }
	| error { info_error 1; EVar "0" } /* need to think how to deal with it! */
;

aexpr: VARIABLE { EVar($1) }
/*	| MINUS NUMBER %prec UNARY_MINUS { ENum(- $2) } */
	| CAPITALNAME { EConstr($1) }
	| NUMBER { ELit(LNum($1)) }
/*	| CONSTRUCTOR expr { EConstr($2) } */
	| LPAREN expr RPAREN { $2 }
;

defns: defn SEMICOLON defns { $1 :: $3 }
	| defn { [$1] }
;

defn: VARIABLE ASSIGNMENT expr { ($1, $3) }
;

alts: alt /*SEMICOLON*/ COMMA alts { $1 :: $3 }
	| alt { [$1] }
;

alt: pattern ALT_ARROW expr 
	{ ($1, $3) }
;

pattern: /* UNDERSCORE { PWildcard } */
	| VARIABLE { PVar($1) }
	| NUMBER { PLit(LNum($1)) }
	| TRUE { PLit(LBool(true)) }
	| FALSE { PLit(LBool(false)) }
	| CAPITALNAME patternsz { PConstr($1, $2) }
;

patternsz: /* empty */ { [] }
	| pattern patternsz { $1 :: $2 }
;

varsz: /* empty */ { [] }
	| VARIABLE varsz { $1 :: $2 }
;

vars: VARIABLE varsz { $1 :: $2 }
;

%% (* trailer - additional OCaml code *)


