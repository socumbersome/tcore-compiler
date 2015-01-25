
open Tcore_types;;

let rec transformExpr expr = match expr with
	| EVar' x -> Core_types.EVar x
	| ELit'(LNum i) -> Core_types.ENum i
	| ELit'(LBool b) -> if b then Core_types.EVar "true"
		else Core_types.EVar "false"
	| EConstr'(s, id, tag, arity) -> Core_types.EConstr(tag, arity)
	| EAppl'(e1, e2) -> let e1' = transformExpr e1
		in let e2' = transformExpr e2
		in Core_types.EAppl(e1', e2')
	| ELet'(isrec, defs, e) -> let defs' = List.map (fun (n, e) ->
		(n, transformExpr e)) defs
		in let e' = transformExpr e
		in Core_types.ELet(isrec, defs', e')
	| ECase'(e, alts) -> let e' = transformExpr e
		in let alts' = List.map (fun (ci, vars, e) ->
			let (_, _, tag, arity) = ci
			in let e' = transformExpr e
			in (tag, vars, e')
		) alts
		in Core_types.ECase(e', alts')
	| ELambd'(vars, e) -> let e' = transformExpr e
		in Core_types.ELambd(vars, e')
	;;

let transform program =
	List.map (fun (name, vars, body) ->
		let body' = transformExpr body
		in (name, vars, body')
	) program
	;;
