(* unfortunately, having code like:

downfrom n = if n == 0 then Pack{1,0}
	else Pack{2,2} n (downfrom (n-1))

doesn't work in Core because Packs are
treated in special way - therefore we need
to lift all packs into supercombinators,
i.e. above code will look like:

*pack1,0 = Pack{1,0};
*pack2,2 = Pack{2,2};
downfrom n = if n == 0 then *pack1,0
	else *pack2,2 n (downfrom (n-1))

Note that there is `*` character in names
which is illegal in Core source programs,
so there won't be any name clashes.
*)

open Core_types;;
open Lists;;

let mkName t a = "*pack" ^ string_of_int t ^
	"," ^ string_of_int a;;

let rec transformExpr acc expr = match expr with
	| EVar _ -> (acc, expr)
	| ENum _ -> (acc, expr)
	| EConstr(tag, arity) -> let pname = mkName tag arity
		in let e' = EVar pname
		in if List.exists (fun (n, _, _) ->
			n = pname) acc
		then
			(acc, e')
		else
			let acc' = (pname, [], expr)::acc
			in (acc', e')
	| EAppl(e1, e2) -> let (acc1, e1') = transformExpr acc e1
		in let (acc2, e2') = transformExpr acc1 e2
		in (acc2, EAppl(e1', e2'))
	| ELet(isrec, defs, e) ->
		let (acc', defs') = mapAccuml (fun acc (n, e) ->
			let (acc', e') = transformExpr acc e
			in (acc', (n, e'))
		) acc defs
		in let (acc1, e') = transformExpr acc' e
		in (acc1, ELet(isrec, defs', e'))
	| ECase(e, alts) ->
		let (acc', alts') = mapAccuml (fun acc (t, vs, e) ->
			let (acc', e') = transformExpr acc e
			in (acc', (t, vs, e'))
		) acc alts
		in let (acc1, e') = transformExpr acc' e
		in (acc1, ECase(e', alts'))
	| ELambd(vars, e) ->
		let (acc', e') = transformExpr acc e
		in (acc', ELambd(vars, e'))
	;;

let packLift program =
	let (newsc, trsc) = mapAccuml (fun newscs (name, vars, body) ->
		let (nscs, tre) = transformExpr newscs body
		in (nscs, (name, vars, tre))
	) [] program
	in newsc @ trsc;;
