(* transforms program from using tExpr to tExpr' *)

open Miscellaneous;;
open Lists;;
open Tcore_types;;

exception UndefTyConstr of string;;

let allVarsOfPattern ps = List.map (fun p -> match p with
	| PVar n -> n
	| _ -> failwith "pattern of depth exactly 1 expected"
	) ps;;

let rec transformExpr tc2i expr = match expr with
	| EVar x -> EVar' x
	| ELit n -> ELit' n
	| EConstr name -> (match aLookup tc2i name with
	(* this doesn't work for constructors with arguments!
	they're treated like functions now and it entails problems
	for type inference - instead having its own type,
	typechecker infers function type, which is wrong!
	TODO: fix it! either here or in typechecker... *)
		| Some i -> EConstr' i
		| None -> raise (UndefTyConstr ("Undefined type constructor "
		^ name))
		)
	| EAppl(e1, e2) -> let e1' = transformExpr tc2i e1
		in let e2' = transformExpr tc2i e2
		in EAppl'(e1', e2')
	| ELet(isrec, defs, body) -> let defs' = List.map
		(fun (dn, de) -> (dn, transformExpr tc2i de)) defs
		in let body' = transformExpr tc2i body
		in ELet'(isrec, defs', body')
	| ECase(e, alts) -> let alts' = List.map (fun (p, e) -> match p with
		| PLit l -> failwith "case plit not implemented yet"
		| PVar v -> failwith "case pvar not implemented yet"
		| PConstr(name, ps) -> (match aLookup tc2i name with
			| Some i -> (i, allVarsOfPattern ps, transformExpr tc2i e)
			| None -> raise (UndefTyConstr ("Undefined type constructor "
			^ name))
		)) alts
		in let e' = transformExpr tc2i e
		in ECase'(e', alts')
	| ELambd(vars, body) -> let body' = transformExpr tc2i body
		in ELambd'(vars, body')
	;;

let transformTd tc2i i2t tid ((tname, vars), adts) =
	let (tc2i', i2t', _) = List.fold_left (fun (tc2i, i2t, tag) (name, args) ->
		let i = (0, tid, tag, List.length args)
		in ((name, i)::tc2i, (i, (tname, List.length vars))::i2t, tag + 1)
	) (tc2i, i2t, 1) adts
	in (tc2i', i2t', tid + 1)
	;;

let transform program =
	let (scs, tc2i, i2t, tid) =
	List.fold_left (fun (scs, tc2i, i2t, tid) d -> match d with
		| Supercombinator (n, vs, body) -> let body' = transformExpr tc2i body
			in ((n, vs, body')::scs, tc2i, i2t, tid)
		| TypeDefinition td -> 
			let (tc2i', i2t', tid') = transformTd tc2i i2t tid td
			in (scs, tc2i', i2t', tid')
	) ([], [], [], 0) program
	(* acc is (supercominators list, 
		a map from type constructor names to their constrInfo,
		a map from constrInfo to type's name and its number of arguments
		global id for telling types apart) *)
	in (List.rev scs, tc2i, i2t)
	;;
