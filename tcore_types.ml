type tName = string;;
type isRecursive = bool;;
type 'a tExpr =
	  EVar of tName (* could be "x" as well as "+"! *)
	| ELit of lit
	| EConstr of tName
	| EAppl of 'a tExpr * 'a tExpr (* function application *)
	| ELet of 
		isRecursive	(* True iff recursive *)
		* ('a * 'a tExpr) list (* definitions *)
		* 'a tExpr (* body of let[rec] *)
	| ECase of 'a tExpr * 'a tAlter list
	| ELambd of 'a list * 'a tExpr (* lambda abstractions *)
and
	lit = LNum of int | LBool of bool
and
	'a tAlter = pattern * 'a tExpr
and pattern = 
	  PLit of lit
	| PVar of tName
	| PConstr of tName * pattern list
(*	| PWildcard (* i.e. '_' *) *)
	;;

type constrInfo = int * int * int * int;;
(* (s, id, tag, arity), where
	s - "special tag" with s=0 for user-defined types and s>0
		for built-ins (in current implementation, not used!)
	id - an unique identifier of the type that this constructor creates
	tag - an unique identifier of the constructor among all
		constructors for that type
	arity - arity
*)

type 'a tExpr' = (* after transforming the form of type constructors *)
	  EVar' of tName
	| ELit' of lit
	| EConstr' of constrInfo
	| EAppl' of 'a tExpr' * 'a tExpr'
	| ELet' of
		isRecursive
		* ('a * 'a tExpr') list
		* 'a tExpr'
	| ECase' of 'a tExpr' * 'a tAlter' list
	| ELambd' of 'a list * 'a tExpr'
and
	'a tAlter' = constrInfo * 'a list * 'a tExpr'
	(* note that with that definition we don't allow nested patterns
	TODO: change it and implement nested patterns functionality
		(that change will affect Core language as well since as of
		current time, Core doesn't allow its nested patterns either)
	*)
	;;

let recursive = true;;
let nonRecursive = false;;

type tcoreExpr = tName tExpr;;
type tcoreAlter = tName tAlter;;

type 'a typeIntr = tName * 'a list;;

type 'a adtArg =
	  Type of 'a typeIntr
	| TVar of 'a
	;;

type 'a adt = tName * 'a adtArg list;;

type 'a typeDef = 'a typeIntr * 'a adt list;;

type 'a supCombDef = tName * 'a list * 'a tExpr;;
type tcoreSupCombDef = tName supCombDef;;

type 'a topdef = 
	  Supercombinator of 'a supCombDef
	| TypeDefinition of 'a typeDef
	;;

type 'a program = ('a topdef) list;;
type tcoreProgram = tName program;;

