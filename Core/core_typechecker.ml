
(** DEPRECATED !!! *)

open Core_types;;
open Core;;
open Core_AST_utils;;
open Unique_names;;
open Set;;
open Miscellaneous;;
open Lists;;

exception TypecheckerError of string;;

module VNSet = Set.Make(String);;

type typeVarName = string;;

type typeExpr = 
	  TypeVar of typeVarName * (typeExpr option)
	(* type variable can represent some other type, hence, typeExpr option *)
	| TypeOp of string * typeExpr list;;

type typeEnv = (cName, typeExpr) assoc;;

type typeInstanceEnv = (typeVarName, typeExpr) assoc;;

type nonGeneric = VNSet.t;;

type state = State of nameSupply * typeInstanceEnv;;
let getStateTypeInstanceEnv (ns, tie) = tie;;
let putStateTypeInstanceEnv tie (ns, _) = (ns, tie);;
let getStateNameSupply (ns, tie) = ns;;
let putStateNameSupply ns (_, tie) = (ns, tie);;

type 'a typedExpr = typeExpr * 'a typedExpr'

and 'a typedExpr' =
	  TVar of cName
	| TNum of int
	| TConstr of int * int
	| TAppl of 'a typedExpr * 'a typedExpr
	| TLet of cIsRec * 'a typedDefn list * 'a typedExpr
	| TCase of 'a typedExpr * 'a typedAlt list
	| TLambd of 'a list * 'a typedExpr
and
	'a typedDefn = 'a * 'a typedExpr
and
	'a typedAlt = int * 'a typedExpr
	;;

type 'a typedSupCombDefn = TypedSupCombDefn of 
	cName * 'a typedExpr list * 'a typedExpr;;

type 'a typedProgram = dataType list * 'a typedSupCombDefn list;;

let tarrow t1 t2 = TypeOp "->" [t1; t2];;
let tint = TypeOp "int" [];;
let tbool = TypeOp "bool" [];;
let tcross t1 t2 = TypeOp "cross" [t1; t2];; (* what's that? !! *)
let tlist t = TypeOp "list" [t];;

let rec updateType state tv = match tv with
	| TypeVar(name, repr) -> match aLookup (getStateInstanceEnv state) name with
		| Some t -> updateType state t
		| None -> tv
	| TypeOp(name, args) -> TypeOp(name, List.map (updateType state) args)
	;;

let updateArgs state args = 
	List.map (fun (typeExpr, expr) ->
		(updateType state typeExpr, expr)
	) args;;

let updateInstances state scs =
	List.map (fun TypedSupCombDefn(name, args, expr) ->
		TypedSupCombDefn(name, updateArgs state args, updateExpr state expr)
	) scs;;

let updateExpr state (te, tpde) = match tpde with
	| TVar v -> (updateType state te, tpde)
	| TNum n -> (updateType state te, tpde)
	| TAppl(e1, e2) ->
		let e1' = updateExpr state e1
		in let e2' = updateExpr state e2
		in (updateType state te, TAppl(e1', e2'))
	| TLet(isrec, defns, expr) =
		let defns' = List.map (fun (name, expr) ->
			(name, updateExpr state expr)
		) defns
		in let expr' = updateExpr state expr
		in (updateType state te, TLet(isrec, defns', expr'))
	| TCase(expr, alts) ->
		(updateType state te, updateCase state expr alts
	| TLambd(args, expr) ->
		let expr' = updateExpr state expr
		in (updateType state te, TLambd(args, expr'))
	| TConstr(tag, arity) as whole ->
		(updateType state te, whole)

and updateCase state expr alts =
	let expr' = updateExpr state expr
	in let alts' = List.map (fun (t, e) ->
		(t, updateExpr state expr)
	) alts
	in TCase(expr', alts')
	;;

let createTypeVar (state, env) name = 
	let (state', tv) = newTypeVariable state
	in let env' = (name, tv)::env
	in ((state', env'), (tv, TVar name));;

let rec typeCheckExpr state env nonGeneric expr = match expr with
	| EVar v -> let (state', typeExpr) = getType state env nonGeneric v
		in (state', (typeExpr, TVar v))
	| ENum n -> (state, (tint, TNum n))
	| EAppl(e1, e2) ->
		let (state1, e1') = typeCheckExpr state env nonGeneric e1
		in let (state2, e2') = typeCheckExpr state1 env nonGeneric e2
		in let (state3, resType) = newTypeVariable state2
		in let (funType, _) = e1'
		in let (argType, _) = e2'
		in let (state4, _, _) = unify state3 (tarrow argType resType) funType
		in (state4, (resType, TAppl(e1', e2')))
	| ELet(isrec, defns, body) -> if isrec then
		typeCheckLetrec state env nonGeneric defns body
		else typeCheckLet state env nonGeneric defns body
	| ECase(_, _) -> typeCheckCase state env nonGeneric expr
	| EConstr(_, _) -> typeCheckConstr state env nonGeneric expr
	| ELambd(_, _) -> failwith "nie umie lambd"

and typeCheckCase state env nonGeneric ECase(expr, alts) =
	let (_, firstAltExpr) = List.hd alts
	in let (state1, (firstAltType, _)) = typeCheckExpr
		state env nonGeneric firstAltExpr
	in let ((state2, _), alts') = mapAccuml (typeCheckAlt nonGeneric env)
		(state1, firstAltType) alts
	in let (state3, expr') = typeCheckExpr state2 env nonGeneric expr
	in (state3, (firstAltType, TCase(expr', alts')))

and typeCheckConstr state env nonGeneric EConstr(tag, arity) =
	

let typeCheckSc nonGeneric (state, env) (name, args, expr) =
	(* create a type variable to hold the type of supcomb *)
	let (state0, tv) = newTypeVariable state
	in let env0 = (name, tv)::env
	(* create type variables for supcomb arguments *)
	in let ((state1, env1), args') = mapAccuml
		createTypeVar (state0, env0) args
	(* typecheck supcomb body *)
	in let (state2, expr') = typeCheckExpr state1 env1 nonGeneric expr
	(* unify type for supcomb *)
	in let argTypeVars = List.map fst args'
	in let scType = List.fold_right tarrow te argTypeVars
	in let (state3, _, _) = unify state2 tv scType
	in ((state3, env1), TypedSupCombDefn(name, args', expr'))
	;;

let typeCheck (adts, scs) = 
	let ((state, _), scs') =
		mapAccuml (typeCheckSc initialNonGeneric) initialTypeEnv scs
	in (adts, updateInstances state scs');;
