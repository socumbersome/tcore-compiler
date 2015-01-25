
open Core_types;;
open Core;;
open Gm_types;;
open Heap;;
open Core_AST_utils;;
open Miscellaneous;;

type gmCompiledSC = (cName * int * gmCode);;
type gmEnvironment = (cName, int) Lists.assoc;;
type gmCompiler = coreExpr -> gmEnvironment -> gmCode;;

let primitives = [
	("+", ["x"; "y"], EAppl(EAppl(EVar "+", EVar "x"), EVar "y"));
	("-", ["x"; "y"], EAppl(EAppl(EVar "-", EVar "x"), EVar "y"));
	("*", ["x"; "y"], EAppl(EAppl(EVar "*", EVar "x"), EVar "y"));
	("/", ["x"; "y"], EAppl(EAppl(EVar "/", EVar "x"), EVar "y"));
	("neg", ["x"], EAppl(EVar "neg", EVar "x"));
	("==", ["x"; "y"], EAppl(EAppl(EVar "==", EVar "x"), EVar "y"));
	("!=", ["x"; "y"], EAppl(EAppl(EVar "!=", EVar "x"), EVar "y"));
	(">=", ["x"; "y"], EAppl(EAppl(EVar ">=", EVar "x"), EVar "y"));
	(">", ["x"; "y"], EAppl(EAppl(EVar ">", EVar "x"), EVar "y"));
	("<=", ["x"; "y"], EAppl(EAppl(EVar "<=", EVar "x"), EVar "y"));
	("<", ["x"; "y"], EAppl(EAppl(EVar "<", EVar "x"), EVar "y"));
	("if", ["c"; "t"; "f"],
		EAppl(EAppl(EAppl(EVar "if", EVar "c"), EVar "t"), EVar "f"));
	("|", ["x"; "y"], EAppl(EAppl(EVar "|", EVar "x"), EVar "y"));
	("&", ["x"; "y"], EAppl(EAppl(EVar "&", EVar "x"), EVar "y"));
	("not", ["x"], EAppl(EVar "not", EVar "x"));
	("true", [], EConstr(2, 0));
	("false", [], EConstr(1, 0))
	];;

let compiledPrimitives = [];;

let availableArithmeticBinary =
	["+"; "-"; "*"; "/" ];;
let availableComparisonBinary =
	["=="; "!="; "<"; "<="; ">"; ">=" ];;
let availableLogicalBinary =
	["|"; "&"];;

let builtInBinary = [ ("+", Add); ("-", Sub); ("*", Mul);
	("/", Div); ("==", Eq); ("!=", Ne); ("<", Lt);
	("<=", Le); (">", Gt); (">=", Ge);
	("|", Or); ("&", And) ];;

let argOffset n env = List.map (fun (v, m) -> (v, m + n)) env;;

let rec compileExprAscEnv comp exs env = match exs with
	| [] -> []
	| e::es -> 
		comp e env @ compileExprAscEnv comp es (argOffset 1 env)
	;;

let rec compileExprConstEnv comp exs env = match exs with
	| [] -> []
	| e::es -> 
		comp e env @ compileExprAscEnv comp es env
	;;

let prepareEnv4Let defs env =
	let n = List.length defs
	in let nrevs = List.rev <| Lists.range 0 (n - 1)
	in Lists.zip (List.map fst defs) nrevs @ argOffset n env;;

let rec compileLetArgs defs env = match defs with
	| [] -> []
	| ((name, expr)::defsr) ->
		compileC expr env @ compileLetArgs defsr (argOffset 1 env)

and compileLetrecArgs defs env updno = match defs with
	| [] -> []
	| ((name, expr)::defsr) ->
		compileC expr env @ [Update updno] 
		@ compileLetrecArgs defsr env (updno - 1)

(* compile in non-strict context - C scheme *)
and compileC expr env = match expr with
	| EVar v -> (match Lists.aLookup env v with
		| Some n -> [Push n]
		| None -> [Pushglobal v]
		)
	| ENum n -> [Pushint n]
	| satc when isSaturatedConstr satc ->
	(* I fear the idea of looking for EConstr is really bad
		and inefficient... but can't think of better one now *)
		let (EConstr(tag, arity), args) = dismantleConstr satc
		in compileExprAscEnv compileC (List.rev args) env
			@ [Pack(tag, arity)]
	| EAppl(e1, e2) -> compileC e2 env 
		@ compileC e1 (argOffset 1 env) @ [MkAppl]
	| ELet(isrec, defs, e) -> 
		let env' = prepareEnv4Let defs env
		in let n = List.length defs
		in if isrec then
			let cargs = compileLetrecArgs defs env' (n - 1)
			in [Alloc n] @ cargs @ compileC e env' @ [Slide n]
		else
			let cargs = compileLetArgs defs env
			in cargs @ compileC e env' @ [Slide n]
	| ECase(e, alts) -> raise (GmCompilationError 
		("Cannot compile case expressions in lazy contexts"))
	| ELambd(vars, e) -> raise (GmCompilationError
		("Cannot compile lambda abstractions in lazy contexts"))
	| EConstr(t, a) -> [Pushglobal ("Pack{" ^ string_of_int t
		^ "," ^ string_of_int a ^ "}") ]
	;;

(* D scheme compilation; comp should correspond to A scheme *)
let compileAlts comp alts env =
	List.map (fun (tag, names, body) -> 
		let n = List.length names
		in (tag, comp n body (Lists.zip names (Lists.range 0 (n - 1))
			@ argOffset n env))
	) alts;;

(* compile in strict context - E scheme
 (that means, when run, it will evaluate
 expr to WHNF) *)
let rec compileE expr env = match expr with
	| ENum n -> [Pushint n]
	| ELet(isrec, defs, e) ->
		let env' = prepareEnv4Let defs env
		in let n = List.length defs
		in if isrec then
			let cargs = compileLetrecArgs defs env' (n - 1)
			in [Alloc n] @ cargs @ compileE e env' @ [Slide n]
		else
			let cargs = compileLetArgs defs env
			in cargs @ compileE e env' @ [Slide n]
	| EAppl(EAppl(EVar op, e1), e2) when 
		List.mem op availableArithmeticBinary ->
		compileB expr env @ [Mkint]
	| EAppl(EAppl(EVar op, e1), e2) when 
		List.mem op (availableComparisonBinary
			@ availableLogicalBinary) ->
		compileB expr env @ [Mkbool]
	| EAppl(EVar "neg", e) ->
		compileB expr env @ [Mkint]
	| EAppl(EVar "not", e) ->
		compileB expr env @ [Mkbool]
	| EAppl(EAppl(EAppl(EVar "if", e0), e1), e2) ->
		compileB e0 env 
		@ [Cond(compileE e1 env, compileE e2 env)]
	| ECase(e, alts) -> compileE e env
		@ [Casejump (compileAlts compileAE alts env)]
	| satc when isSaturatedConstr satc ->
	(* mind you - _not!_ the same code as in compileC *)
		let (EConstr(tag, arity), args) = dismantleConstr satc
		in compileExprConstEnv compileC (List.rev args) env
			@ [Pack(tag, arity)]
	| _ -> compileC expr env @ [Eval]

(* plays role of A scheme compilation *)
and compileAE offset expr env =
	[Split offset] @ compileE expr env @ [Slide offset]

(* when run, evaluates expr to WHNF and
leaves the result on the VStack *)
and compileB expr env = match expr with
	| ENum n -> [Pushbasic n]
	| ELet(isrec, defs, e) -> 
		let env' = prepareEnv4Let defs env
		in let n = List.length defs
		in if isrec then
			let cargs = compileLetrecArgs defs env' (n - 1)
			in [Alloc n] @ cargs @ compileB e env' @ [Pop n]
		else
			let cargs = compileLetArgs defs env
			in cargs @ compileB e env' @ [Pop n]
	| EAppl(EAppl(EVar op, e1), e2) when 
		List.mem op (Lists.aDomain builtInBinary) ->
		let Some ibin = Lists.aLookup builtInBinary op
		in compileB e2 env @ compileB e1 env @ [ibin]
	| EAppl(EVar "neg", e) ->
		compileB e env @ [Neg]
	| EAppl(EVar "not", e) ->
		compileB e env @ [Not]
	| EAppl(EAppl(EAppl(EVar "if", e0), e1), e2) ->
		compileB e0 env 
		@ [Cond(compileB e1 env, compileB e2 env)]
	| _ -> compileE expr env @ [S2V]
	;;

(* R-strict context *)
let rec compileR expr env d = match expr with
	| ELet(isrec, defs, e) -> 
		let env' = prepareEnv4Let defs env
		in let n = List.length defs
		in if isrec then
			let cargs = compileLetrecArgs defs env' (n - 1)
			in [Alloc n] @ cargs @ compileR e env' (n + d)
		else
			let cargs = compileLetArgs defs env
			in cargs @ compileR e env' (n + d)
	| EAppl(EAppl(EAppl(EVar "if", e0), e1), e2) ->
		compileB e0 env 
		@ [Cond(compileR e1 env d, compileR e2 env d)]
	| ECase(e, alts) -> compileE e env
		@ [Casejump (compileAlts (compileAR d) alts env)]
	| _ -> compileE expr env @ [Update d; Pop d; Unwind]

and compileAR d offset expr env =
	[Split offset] @ compileR expr env (offset + d);;

let compileSc (name, varsn, body) =
	let n = List.length varsn
	in (name, n, compileR
		body 
		(Lists.zip varsn (Lists.range 0 (n - 1)))
		n
	);;

let allocateSc heap (name, nargs, instrs) =
	let (heap', addr) = hAlloc heap (NGlobal(nargs, instrs))
	in (heap', (name, addr));;

let buildInitialHeap program =
	let compiled = List.map compileSc
		(preludeDefs @ program @ primitives)
	in Lists.mapAccuml allocateSc hInitial
		(compiled @ compiledPrimitives);;

let initialCode = [Pushglobal "main"; Eval; Print];;

let compile program =
	let (heap, globals) = buildInitialHeap program
	in ("", initialCode, [], [], [], heap, globals, statInitial);;
