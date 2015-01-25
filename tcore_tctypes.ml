
open Tcore_types;;
open Lists;;

type tvar = TV of string;;

type tajp =
	  TVar of tvar
	| TCon of string
	| TArr of tajp * tajp
	;;

let isArrow t = match t with
	| TArr _ -> true
	| _ -> false
	;;

let typeNum = TCon "Num";;
let typeBool = TCon "Bool";;

type scheme = Forall of tvar list * tajp;;

type typeEnv = TypeEnv of (tName, scheme) assoc;;

let emptyTyenv = TypeEnv aEmpty;;

let typeof (TypeEnv env) name = aLookup env name;;

type subst = (tvar, tajp) assoc;;

let nullSubst = aEmpty;;
