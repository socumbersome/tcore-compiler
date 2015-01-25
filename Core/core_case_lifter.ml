(** transforms Core program so that ECase
 expressions appearing in non-strict contexts
 are turned into supercombinator applications
 (creating new supercombinator definitions
 as a result)
*)

(* much could be written in less code (for instance,
 liftE' equals liftE), BUT I leave it as it is because it
 nicely mimics compilation schemes in G-machine - and
 it was G-machine compiler that firstly suggested
 lifting Case expressions - so, this "boilerplate code"
 is kind of like paying homage to G-machine :P *)

(* TODO: what we do here seems extremely monadic, so
 it would be good to rewrite it in a monadic style
 (state monad...) *)

open Core_types;;
open Core_AST_utils;;
open Unique_names;;

let strictBinaryOps = ["+"; "-"; "*"; "/"; "=="; "!=";
	"<"; "<="; ">"; ">=" ];;

let liftOfMany liftScheme exs acc =
	Lists.mapAccuml (fun acc e ->
		let (ns', scs', e') = liftScheme e acc
		in ((ns', scs'), e')
	) acc exs;;

let rec liftLet' defs acc =
	Lists.mapAccuml (fun acc (name, e) ->
		let (ns', scs', e') = liftC e acc
		in ((ns', scs'), (name, e'))
	) acc defs

and liftLet liftScheme defs expr acc =
	let ((ns', scs'), defs') = liftLet' defs acc
	in let (ns'', scs'', expr') = liftScheme expr (ns', scs')
	in (ns'', scs'', (defs', expr'))

and liftLetrec' defs acc =
	Lists.mapAccuml (fun acc (name, e) ->
		let (ns', scs', e') = liftC e acc
		in ((ns', scs'), (name, e'))
	) acc defs

and liftLetrec liftScheme defs expr acc =
	let ((ns', scs'), defs') = liftLetrec' defs acc
	in let (ns'', scs'', expr') = liftScheme expr (ns', scs')
	in (ns'', scs'', (defs', expr'))

(* in non-strict context *)
and liftC expr ((ns, scs) as acc) = (match expr with
	| EVar v -> (ns, scs, expr)
	| ENum _ -> (ns, scs, expr)
	| satc when isSaturatedConstr satc ->
		let (EConstr(tag, arity) as econstr, args) = dismantleConstr satc
		in let ((ns', scs'), args') = liftOfMany liftC args acc
		in let nexpr = List.fold_left
			(fun e ei -> EAppl(e, ei))
			econstr
			args'
		in (ns', scs', nexpr)
	| EAppl(e1, e2) ->
		let (ns', scs', e2') = liftC e2 acc
		in let (ns'', scs'', e1') = liftC e1 (ns', scs')
		in (ns'', scs'', EAppl(e1', e2'))
	| ELet(isrec, defs, e) -> if isrec then
		let (ns', scs', (defs', e')) = liftLetrec liftC defs e acc
		in (ns', scs', ELet(isrec, defs', e'))
		else
		let (ns', scs', (defs', e')) = liftLet liftC defs e acc
		in (ns', scs', ELet(isrec, defs', e'))
	| ECase(e, alts) ->
		let (ns1, scs1, e') = liftE e acc
		in let ((ns2, scs2), alts') = liftAlts liftE' alts (ns1, scs1)
		in let (ns3, casename) = getName ns2 "Case#"
		in let scCase = (casename, ["x"], 
			ECase(EVar "x", alts'))
		in (ns3, scCase::scs2, EAppl(EVar casename, e'))
		(* now, as we lifted Case, its parts should be
		 lifted starting from E (strict context) *)
	| ELambd(vars, e) -> failwith ("Lambda abstraction "
	^ "encountered during case lifting")
	| EConstr(t, a) -> (ns, scs, expr)
	)

and liftAlts liftScheme alts acc =
	Lists.mapAccuml (fun acc (tag, names, body) ->
		let (ns', scs', e') = liftScheme body acc
		in ((ns', scs'), (tag, names, e'))
	) acc alts

(* in strict context *)
and liftE expr ((ns, scs) as acc) = match expr with
	| ENum n -> (ns, scs, expr)
	| ELet(isrec, defs, e) -> if isrec then
		let (ns', scs', (defs', e')) = liftLetrec liftE defs e acc
		in (ns', scs', ELet(isrec, defs', e'))
		else
		let (ns', scs', (defs', e')) = liftLet liftE defs e acc
		in (ns', scs', ELet(isrec, defs', e'))
	| EAppl(EAppl(EVar op, e1), e2) when 
		List.mem op strictBinaryOps ->
		let (ns', scs', e2') = liftE e2 acc
		in let (ns'', scs'', e1') = liftE e1 (ns', scs')
		in (ns', scs', EAppl(EAppl(EVar op, e1'), e2'))
	| EAppl(EVar "neg", e) -> 
		let (ns', scs', e') = liftE e acc
		in (ns', scs', EAppl(EVar "neg", e'))
	| EAppl(EAppl(EAppl(EVar "if", e0), e1), e2) ->
		let (ns0, scs0, e0') = liftE e0 acc
		in let (ns1, scs1, e1') = liftE e1 (ns0, scs0)
		in let (ns2, scs2, e2') = liftE e2 (ns1, scs1)
		in (ns2, scs2, EAppl(EAppl(EAppl(EVar "if", e0'), e1'), e2'))
	| ECase(e, alts) ->
		let (ns', scs', e') = liftE e acc
		in let ((ns'', scs''), alts') = liftAlts liftE' alts (ns', scs')
		in (ns'', scs'', ECase(e', alts'))
	| satc when isSaturatedConstr satc ->
		let (EConstr(tag, arity) as econstr, args) = dismantleConstr satc
		in let ((ns', scs'), args') = liftOfMany liftC args acc
		in let nexpr = List.fold_left
			(fun e ei -> EAppl(e, ei))
			econstr
			args'
		in (ns', scs', nexpr)
	| _ -> liftC expr acc

and liftE' expr acc = liftE expr acc;;

(* acc in liftX is a pair 
(name_supply, list of brand new supercombinators so far)
and each liftX returns a triple
(name_supply, list of brand new supercombinators,
	an expression of possibly altered itself)
*)

let liftProgram program =
	let (_, css) = Lists.mapAccuml (fun ns scdef ->
		let (name, varsn, body) = scdef
		in let (ns', newscs, newexpr) = liftE body (ns, [])
		in (ns', (name, varsn, newexpr)::newscs)
	) Unique_names.initialSupply program
	in List.flatten css;;
