
open Core_types;;
open Core;;
open Core_AST_utils;;
open Unique_names;;
open Set;;
open Miscellaneous;;

let freeVarsOf (free_vars, expr) = free_vars;;

let freeVarsOf_alt (tag, args, rhs) = CNameSet.(
	diff (freeVarsOf rhs) (of_list args));;

(* freeVars_e : 
	cNameSet.t	(* local vars - candidates for free variables *) 
	-> cExpr	(* expression to annotate *)
	-> (cName * cNameSet.t) annExpr
*)
let rec freeVars_e lvars expr = match expr with
	| ENum n -> (CNameSet.empty, ANum n)
	| EVar v -> if CNameSet.mem v lvars then
		(CNameSet.singleton v, AVar v)
		else (CNameSet.empty, AVar v)
	| EAppl(e1, e2) ->
		let e1' = freeVars_e lvars e1
		in let e2' = freeVars_e lvars e2
		in (CNameSet.union (freeVarsOf e1') (freeVarsOf e2'),
				AAppl(e1', e2'))
	| ELambd(args, body) ->
		let new_lvars = CNameSet.(union lvars (of_list args))
		in let body' = freeVars_e new_lvars body
		in CNameSet.(
			(diff (freeVarsOf body') (of_list args),
			ALambd(args, body'))
		)
	| ELet(isrec, defns, body) ->
		CNameSet.(
		let binders = bindersOf defns
		in let binderSet = of_list binders
		in let body_lvars = union lvars binderSet
		in let rhs_lvars = if isrec then body_lvars
			else lvars
		in let rhss' = List.map (freeVars_e rhs_lvars)
			(rhssOf defns)
		in let defns' = Lists.zip binders rhss'
		in let freeInValues = List.fold_left union empty
			(List.map freeVarsOf rhss')
		in let defnsFree = if isrec then
			diff freeInValues binderSet
			else freeInValues
		in let body' = freeVars_e body_lvars body
		in let bodyFree = diff (freeVarsOf body') binderSet
		in (union defnsFree bodyFree,
			ALet(isrec, defns', body'))
		)
	| ECase(e, alts) -> freeVars_case lvars e alts
	| EConstr(tag, arity) -> (CNameSet.empty, AConstr(tag, arity))

and freeVars_case lvars e alts = CNameSet.(
	let e' = freeVars_e lvars e
	in let (alts_lvars, alts') = Lists.mapAccuml
		(fun lvars (tag, args, expr) ->
			let new_lvars = CNameSet.(union lvars (of_list args))
			in let expr' = freeVars_e new_lvars expr
			in let expr_lv = diff (freeVarsOf expr') (of_list args)
			in (union lvars expr_lv,
				(tag, args, expr'))
		) lvars alts
	in (union (freeVarsOf e') alts_lvars,
		ACase(e', alts'))
	);;

let freeVars prog = List.map (fun (name, args, body) ->
	(name, args, freeVars_e (CNameSet.of_list args) body))
	prog;;

let rec abstract_e = function
	| (free, AVar v) -> EVar v
	| (free, ANum k) -> ENum k
	| (free, AAppl(e1, e2)) -> EAppl(abstract_e e1, abstract_e e2)
	| (free, ALet(isrec, defns, body)) ->
		ELet(isrec, List.map 
			(fun (name, body) -> (name, abstract_e body))
			defns,
			abstract_e body)
	| (free, ALambd(args, body)) -> CNameSet.(
		let fvarsList = fold (fun el xs -> el::xs) free []
		in let sc_rhs = ELambd(fvarsList @ args, abstract_e body)
		in let sc = ELet(nonRecursive, [("$sc", sc_rhs)], (EVar "$sc"))
		in List.fold_left (fun e1 e2 -> EAppl(e1, e2)) sc
			(List.map (fun v -> EVar v) fvarsList)
		)
	| (free, AConstr(tag, arity)) -> EConstr(tag, arity)
	| (free, ACase(e, alts)) -> abstract_case free e alts

and abstract_case free e alts = 
	ECase(abstract_e e, List.map
		(fun (tag, args, expr) ->
			(tag, args, abstract_e expr))
		alts)
;;

let abstract prog = List.map (fun (sc_name, args, rhs) ->
	(sc_name, args, abstract_e rhs)) prog;;

let newNames ns old_names =
	let (ns', new_names) = getNames ns old_names
	in let env = Lists.zip old_names new_names
	in (ns', new_names, env);;

(* ns stands for 'name supply'
env maps old names to new names *)
let rec rename_e env ns expr = match expr with
	| ENum n -> (ns, ENum n)
	| EVar v -> (match Lists.aLookup env v with
		| Some v' -> (ns, EVar v')
		| None -> (ns, expr)
		)
	| EAppl(e1, e2) ->
		let (ns1, e1') = rename_e env ns e1
		in let (ns2, e2') = rename_e env ns1 e2
		in (ns2, EAppl(e1', e2'))
	| ELambd(args, body) ->
		let (ns1, args', env') = newNames ns args
		in let (ns2, body') = rename_e (env' @ env) ns1 body
		in (ns1, ELambd(args', body'))
	| ELet(isrec, defns, body) ->
		let binders = bindersOf defns
		in let (ns1, binders', env') = newNames ns binders
		in let body_env = env' @ env
		in let rhsEnv = if isrec then body_env else env
		in let (ns2, rhss') = Lists.mapAccuml
			(rename_e rhsEnv)
			ns1
			(rhssOf defns)
		in let (ns3, body') = rename_e body_env ns2 body		
		in (ns3, ELet(
			isrec,
			(Lists.zip binders' rhss'),
			body'))
	| EConstr(tag, arity) -> (ns, expr)
	| ECase(e, alts) -> rename_case env ns e alts

and rename_case env ns e alts = CNameSet.(
	let (ns1, e') = rename_e env ns e
	in let (ns_fin, alts') = Lists.mapAccuml
		(fun ns (tag, args, expr) ->
			let (ns', args', env') = newNames ns args
			in let (ns'', expr') = rename_e (env' @ env) ns' expr
			in (ns'', (tag, args', expr'))
		) ns1 alts
	in (ns_fin, ECase(e', alts'))
	);;

let isELambd = function
	| ELambd(_, _) -> true
	| _ -> false
	;;

let rec collectSCs_e expr = match expr with
	| ENum k -> ([], expr)
	| EVar v -> ([], expr)
	| EAppl(e1, e2) ->
		let (scs1, e1') = collectSCs_e e1
		in let (scs2, e2') = collectSCs_e e2
		in (scs1 @ scs2, EAppl(e1', e2'))
	| ELambd(args, body) ->
		let (scs, body') = collectSCs_e body
		in (scs, ELambd(args, body'))
	| EConstr(tag, arity) -> ([], expr)
	| ECase(e, alts) ->
		let (scs_e, e') = collectSCs_e e
		in let (scs_alts, alts') = Lists.mapAccuml
			collectSCs_alt
			[]
			alts
		in (scs_e @ scs_alts, ECase(e', alts'))
	| ELet(isrec, defns, body) ->
		let (rhss_scs, defns') = Lists.mapAccuml
			collectSCs_d
			[]
			defns
		in let scs' = List.filter (isELambd << snd) defns'
		in let non_scs' = List.filter (not << isELambd << snd) defns'
		in let local_scs = List.map
			(fun (name, ELambd(args, body)) ->
				(name, args, body))
			scs'
		in let (body_scs, body') = collectSCs_e body
		in let allscs = rhss_scs @ body_scs @ local_scs
		in if non_scs' = [] then
			(allscs, body')
		else (allscs, ELet(isrec, non_scs', body'))
		
and collectSCs_alt scs (tag, args, rhs) =
	let (scs_rhs, rhs') = collectSCs_e rhs
	in (scs @ scs_rhs, (tag, args, rhs'))

and collectSCs_d scs (name, rhs) =
	let (rhs_scs, rhs') = collectSCs_e rhs
	in (scs @ rhs_scs, (name, rhs'))
	;;

let collectSCs prog =
	let collect_one_sc (sc_name, args, rhs) =
		let (scs, rhs') = collectSCs_e rhs
		in (sc_name, args, rhs') :: scs
	in List.flatten <| List.map collect_one_sc prog;;

let rename prog =
	let rename_sc ns (sc_name, args, rhs) =
		let (ns1, args', env) = newNames ns args
		in let (ns2, rhs') = rename_e env ns1 rhs
		in (ns2, (sc_name, args', rhs'))
	in snd <| Lists.mapAccuml rename_sc initialSupply prog
	;;

let lambdaLift = collectSCs << rename << abstract << freeVars;;
