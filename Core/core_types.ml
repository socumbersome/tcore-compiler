type cName = string;;
type cIsRec = bool;;
type 'a cExpr =
	  EVar of cName (* could be "x" as well as "+"! *)
	| ENum of int
	| EConstr of int * int (* EConstr(tag, arity) *)
	| EAppl of 'a cExpr * 'a cExpr (* function application *)
	| ELet of 
		cIsRec	(* bool with True iff recursive *)
		* ('a * 'a cExpr) list (* definitions *)
		* 'a cExpr (* body of let[rec] *)
	| ECase of 'a cExpr * 'a cAlter list
	| ELambd of 'a list * 'a cExpr (* lambda abstractions *)
and
	'a cAlter = int * 'a list * 'a cExpr (* int for a tag *)
	;;

type coreExpr = cName cExpr;;
type coreAlter = cName cAlter;;

type ('a, 'b) annExpr = ('b * ('a, 'b) annExpr')

and ('a, 'b) annExpr' =
	  AVar of cName
	| ANum of int
	| AConstr of int * int
	| AAppl of (('a, 'b) annExpr) * (('a, 'b) annExpr)
	| ALet of cIsRec
		* (('a, 'b) annDefn) list
		* ('a, 'b) annExpr
	| ACase of (('a, 'b) annExpr)
		* (('a, 'b) annAlter) list
	| ALambd of 'a list * (('a, 'b) annExpr)
and
	('a, 'b) annDefn = ('a * ('a, 'b) annExpr)
and
	('a, 'b) annAlter = (int * 'a list * ('a, 'b) annExpr)
	;;

type ('a, 'b) annProgram = (cName * 'a list * ('a, 'b) annExpr);;

module CNameSet = Set.Make(String);;

(* a `function` f is a supercombinator iff 1) or 2), where
1) it is constant
2) it has no free variables and every inner function
(or, lambda abstraction) in the body of f is
also a supercombinator *)
type 'a supCombDef = cName * 'a list * 'a cExpr;;
type coreSupCombDef = cName supCombDef;;

type 'a program = ('a supCombDef) list;;
type coreProgram = cName program;;

let recursive:cIsRec = true;;
let nonRecursive:cIsRec = false;;
