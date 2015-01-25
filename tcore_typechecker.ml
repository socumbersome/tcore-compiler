
open Miscellaneous;;
open Lists;;
open Unique_names;;
open Tcore_types;;
open Tcore_tctypes;;
open Tcore_printer;;

exception TypecheckerError of string;;
exception TypecheckerTypeError of string;;

let freshTypeVar ns =
	let (ns', ts) = getName ns "a"
	in (ns', TVar (TV ts));;

let tbuiltInBinary =
	let t l r = TArr(l, TArr(l, r))
	in [
	("+", t typeNum typeNum);
	("-", t typeNum typeNum);
	("*", t typeNum typeNum);
	("/", t typeNum typeNum);
	("==", t typeNum typeBool);
	("<", t typeNum typeBool);
	("<=", t typeNum typeBool);
	(">", t typeNum typeBool);
	(">=", t typeNum typeBool);
	("!=", t typeNum typeBool);
	("|", t typeBool typeBool);
	("&", t typeBool typeBool)
	];;

let tbuiltInUnary =
	let t l r = TArr(l, r)
	in [
	("not", t typeBool typeBool);
	("neg", t typeNum typeNum)
	];;

let tbuiltInConsts = [("true", typeBool); ("false", typeBool)];;

let rec functionType xs res = match xs with
	| y::ys -> TArr(y, functionType ys res)
	| [] -> res;;

let extend (TypeEnv env) ((x, s) as p) =
	let env_wo_x = aRmvKey x env
	in TypeEnv (p::env_wo_x);;

(* applies substitution s on type t *)
let rec tapply s t = match t with
	| TCon a -> t
	| TVar a -> aLookupWithDefault s a t
	| TArr(t1, t2) -> TArr(tapply s t1, tapply s t2)
	;;

let rec tftv t = match t with
	| TCon _ -> []
	| TVar a -> [a]
	| TArr(t1, t2) -> union (tftv t1) (tftv t2)
	;;

let sapply s (Forall(xs, t)) = 
	let s' = List.fold_right aRmvKey xs s
	in Forall(xs, tapply s' t)
	;;

let compose s1 s2 = aUnion (aMap (tapply s1) s2) s1;;

let sftv (Forall(xs, t)) = diff (tftv t) xs;;

let eapply s (TypeEnv env) = TypeEnv (aMap (sapply s) env);;

let lapply ap s ts = (List.map << ap) s ts;;

let lftv ftv ts = foldr (union << ftv) [] ts;;

let eftv (TypeEnv env) = lftv sftv (aRange env);;

let occursCheck a t = List.mem a (tftv t);;

let bind a t = match t with
	| TVar a' when a = a' -> nullSubst
	| _ when occursCheck a t -> raise (TypecheckerTypeError (
	"Infinite type due tu unifying " ^ tvar2str a ^ " and "
	^ type2str t))
	| _ -> aSingleton a t
	;;

let rec unify t1 t2 = match (t1, t2) with
	| (TArr(l, r), TArr(l', r')) ->
		let s1 = unify l l'
		in let s2 = unify (tapply s1 r) (tapply s1 r')
		in compose s2 s1
	| (TVar a, t) -> bind a t
	| (t, TVar a) -> bind a t
	| (TCon a, TCon b) when a = b -> nullSubst
	| _ -> raise (TypecheckerTypeError ("Failed to unify types: "
	^ (type2str t1) ^ " and " ^ (type2str t2)))
	;;

let instantiate ns (Forall(xs, t)) =
		let (ns', xs') = mapAccuml (fun ns x ->
			freshTypeVar ns
		) ns xs
		in let s = zip xs xs'
		in (ns', tapply s t)
	;;

let generalize env t =
	let xs = diff (tftv t) (eftv env)
	in Forall(xs, t);;

let lookupEnv ns (TypeEnv env) x = match aLookup env x with
	| None -> raise (TypecheckerTypeError ("Unbound variable " ^ x))
	| Some scheme -> let (ns', t) = instantiate ns scheme
		in (ns', (nullSubst, t))
	;;

(* ... -> (nameSupply, (subst, tajp)) *)
let rec infer tc2i i2t ns env expr = match expr with
	| EVar' c when List.mem c (aDomain tbuiltInConsts) ->
		let Some ct = aLookup tbuiltInConsts c
		in (ns, (nullSubst, ct))
	| EVar' x -> lookupEnv ns env x
	| ELambd'(xs, e) -> (match xs with
		| [] -> infer tc2i i2t ns env e
		| [y] -> let (ns', tv) = freshTypeVar ns
			in let env' = extend env (y, Forall([], tv))
			in let (ns'', (s1, t1)) = infer tc2i i2t ns' env' e
			in (ns'', (s1, TArr(tapply s1 tv, t1)))
		| y::ys -> let (ns', tv) = freshTypeVar ns
			in let env' = extend env (y, Forall([], tv))
			in let (ns1, (s1, lt)) = infer tc2i i2t ns' env' (ELambd'(ys, e))
			in (ns1, (s1, TArr(tapply s1 tv, lt)))
		)
	| EAppl'(EAppl'(EVar' op, e1), e2) when 
		List.mem op (aDomain tbuiltInBinary) ->
		let (ns1, (s1, t1)) = infer tc2i i2t ns env e1
		in let (ns2, (s2, t2)) = infer tc2i i2t ns1 env e2
		in let (ns3, tv) = freshTypeVar ns2
		in let Some opt = aLookup tbuiltInBinary op
		in let s3 = unify (TArr(t1, TArr(t2, tv))) opt
		in (ns3, (compose s1 (compose s2 s3), tapply s3 tv))
	| EAppl'(EVar' uop, e) when List.mem uop (aDomain tbuiltInUnary) ->
		let (ns1, (s1, t1)) = infer tc2i i2t ns env e
		in let (ns2, tv) = freshTypeVar ns1
		in let Some uopt = aLookup tbuiltInUnary uop
		in let s2 = unify (TArr(t1, tv)) uopt
		in (ns2, (compose s1 s2, tapply s2 tv))
	| EAppl'(EAppl'(EAppl'(EVar' "if", cond), tr), fl) ->
		let (ns1, (s1, t1)) = infer tc2i i2t ns env cond
		in let (ns2, (s2, t2)) = infer tc2i i2t ns1 env tr
		in let (ns3, (s3, t3)) = infer tc2i i2t ns2 env fl
		in let s4 = unify t1 typeBool
		in let s5 = unify t2 t3
		in (ns3, (compose s5 (compose s4 (compose s3 (compose s2 s1))),
			tapply s5 t2))
	| EAppl'(e1, e2) -> let (ns', tv) = freshTypeVar ns
		in let (ns1, (s1, t1)) = infer tc2i i2t ns' env e1
		in let (ns2, (s2, t2)) = infer tc2i i2t ns1 (eapply s1 env) e2
		in let s3 = unify (tapply s2 t1) (TArr(t2, tv))
		in (ns2, (compose s3 (compose s2 s1), tapply s3 tv))
	| ELet'(isrec, defs, exp) when isrec = false ->	
	 (match defs with
		| [] -> infer tc2i i2t ns env exp
		| [(x, e)] -> let (ns1, (s1, t1)) = infer tc2i i2t ns env e
			in let env' = eapply s1 env
			in let t' = generalize env' t1
			in let (ns2, (s2, t2)) = 
				infer tc2i i2t ns1 (extend env' (x, t')) exp
			in (ns2, (compose s1 s2, t2))
		| (x, e)::defs' -> let (ns1, (s1, t1)) = infer tc2i i2t ns env e
			in let env1 = eapply s1 env
			in let t' = generalize env1 t1
			in infer tc2i i2t ns1 
				(extend env1 (x, t')) (ELet'(isrec, defs', expr))
		)
	| ELet'(isrec, defs, exp) when isrec = true ->
	(* not sure about correctness... ! *)
		let ((ns1, env1), tvs) = mapAccuml (fun (ns, env) (n, e) ->
			let (ns', tv) = freshTypeVar ns
			in let env' = extend env (n, Forall([], tv))
			in ((ns', env'), tv)
		) (ns, env) defs
		in let (ns2, env2, subst) = List.fold_left
		(fun (ns, env, subst) (n, e) ->
			let (ns', (s, t)) = infer tc2i i2t ns env e
			in let env' = eapply s env
			in let t' = generalize env' t
			in let env'' = extend env' (n, t')
			in (ns', env'', compose s subst)
		) (ns1, env1, nullSubst) defs
		in let (ns3, (s3, t3)) = infer tc2i i2t ns2 env2 exp
		in (ns3, (compose s3 subst, tapply s3 t3))
	| ELit' lit -> (match lit with
		| LNum _ -> (ns, (nullSubst, typeNum))
		| LBool _ -> (ns, (nullSubst, typeBool))
		)
	| EConstr' ((s, id, tag, arity) as cinfo) -> (match aLookup i2t cinfo with
		| Some (name, _) -> (* (ns, (nullSubst, TCon name)) *)
		(* doesn't check what type do arguments have in this
		type constructor... currently, they're arbitrary type variables
		TODO: refine it! *)
			let (ns', tvs) = List.fold_left (fun (ns, tvs) _ ->
				let (ns', tv) = freshTypeVar ns
				in (ns', tv::tvs)
			) (ns, []) (buildn arity 1)
			in (ns', (nullSubst, functionType tvs (TCon name)))
		| None -> failwith "didn't find appropriate type constructor - impossible!?"
		)
	| ECase'(e, alts) ->
	(* case e of alts, where alts is a list of ai -> ei -- hence ais2eis
		in code *)
	(* NOT sure about correctness... *)	
		let (ns1, (s1, t1)) = infer tc2i i2t ns env e
		(* s1 is irrelevant, isn't it? ! *)
		in let ((ns2, subst), ais2eis) = mapAccuml
			(fun (ns, subst) (ci, vars, e) ->
			let Some (name, arity) = aLookup i2t ci
			in let (ns', env') = List.fold_left (fun (ns, env) v ->
				let (nss, tv) = freshTypeVar ns
				in let env' = extend env (v, Forall([], tv))
				in (nss, env')
			) (ns, env) vars
			in let (ns'', (s', t')) = infer tc2i i2t ns' env' e
			in ((ns'', compose s' subst), (TCon name, (s', t')))
		) (ns1, nullSubst) alts
		in let asubsts = List.map (fun (at, _) -> unify t1 at) ais2eis
		in let asubst = List.fold_left
			(fun a b -> compose b a) nullSubst asubsts
		in let etypes = List.map (snd << snd) ais2eis
		in let fstet = List.hd etypes
		in let subst2 = List.fold_left (fun s eit ->
			let s' = unify fstet eit
			in compose s' s
		) nullSubst (List.tl etypes)
		in (ns2, (compose subst2 (compose asubst subst), tapply subst fstet))
	;;

(* TODO!
should rename ugly type variables to nice ones, i.e. a, b, c... *)
let normalize scheme = scheme;;

let closeOver (sub, ty) =
	let sc = generalize emptyTyenv (tapply sub ty)
	in normalize sc;;

let inferExpr tc2i i2t ns env expr = 
	closeOver << snd <| (infer tc2i i2t ns env expr);;

let rec inferTop tc2i i2t env topdefs = match topdefs with
	| [] -> env
	| (name, varsn, body)::tdfs ->
		let (ns1, tv) = freshTypeVar initialSupply
		in let env1 = extend env (name, Forall([], tv))
		in let ts = inferExpr tc2i i2t ns1 env1 (ELambd'(varsn, body))
		in inferTop tc2i i2t (extend env1 (name, ts)) tdfs
	;;

let typecheck tc2i i2t program = 
	let (TypeEnv lenv) = inferTop tc2i i2t emptyTyenv program
	in List.map (fun (name, _, _) -> match aLookup lenv name with
		| None -> (name, "No type inferred!")
		| Some s -> (name, scheme2str s)
	) program
	;;

