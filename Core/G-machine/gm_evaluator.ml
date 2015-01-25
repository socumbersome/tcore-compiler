
open Gm_types;;
open Heap;;
open Miscellaneous;;

let gmFinal s = (getCode s) = [];;

let pushglobal f state =
	match (Lists.aLookup (getGlobals state) f) with
	| Some a -> putStack (a::getStack state) state
	| None -> if Str.string_match gmPackRegexp f 0 then
	(* TODO: use some library for regexpes because those in
		Str are ugly, thread non-safe, and ugly :P *)
			let tag = int_of_string <| Str.matched_group 1 f
			in let arity = int_of_string <| Str.matched_group 2 f
			in let npack = Pack(tag, arity)
			in let nglob = NGlobal(arity, [npack; Update 0; Unwind])
			in let (h', a') = hAlloc (getHeap state) nglob
			in let s' = putGlobals ((f, a')::getGlobals state) state
			in putHeap h' (putStack (a'::getStack s') s')
		else
			raise (GmEvaluationError ("Undeclared global " ^ f));;

let pushint n state =
	match (Lists.aLookup (getGlobals state) (string_of_int n)) with
		| Some a -> putStack (a::getStack state) state
		| None -> 
			let (heap', a) = hAlloc (getHeap state) (NNum n)
			in let state' = putGlobals 
				((string_of_int n, a)::getGlobals state) state
			in 
			putHeap heap' (putStack (a::getStack state') state')
	;;

let pushbasic n state =
	putVStack (n::getVStack state) state;;

let mkbool state =
	let (t::ns) = getVStack state
	in let (heap', a) = hAlloc (getHeap state) (NConstr(t, []))
	in let s' = putStack (a::getStack state) state
	in putHeap heap' (putVStack ns s');;

let mkint state =
	let (n::ns) = getVStack state
	in let (heap', a) = hAlloc (getHeap state) (NNum n)
	in let s' = putStack (a::getStack state) state
	in putHeap heap' (putVStack ns s');;

let s2v state =
	let (a::ads) = getStack state
	in let s' = putStack ads state
	in match hLookup (getHeap state) a with
		| NConstr(t, []) -> putVStack (t::getVStack s') s'
		| NNum n -> putVStack (n::getVStack s') s'
		| _ -> raise (GmEvaluationError ("NConstr or NNum expected "
		^ " when putting on VStack but found sth else"));;

let mkAppl state =
	let (a1::a2::ads') = getStack state
	in let (heap', a) = hAlloc (getHeap state) (NAppl(a1, a2))
	in putHeap heap' (putStack (a::ads') state);;

let getArg = function
	| NAppl(_, a2) -> a2
	| _ -> raise (GmEvaluationError 
		("Trying to get an argument of non-application node"));;

let push n state =
	let ads = getStack state
	in let an = List.nth ads n
	in putStack (an::ads) state;;

let slide n state =
	let (a::ads) = getStack state
	in putStack (a::Lists.drop n ads) state;;

let update n state = 
	let (a::ads) = getStack state
	in let stack' = ads
	in let an = List.nth ads n
	in let heap' = hUpdate (getHeap state) an (NInd a)
	in putHeap heap' (putStack stack' state);;

let pop n state =
	putStack (Lists.drop n (getStack state)) state;;

let rearrange n heap stack =
	let lstack' = Lazy_lists.tolazy <| 
		List.map (getArg << (hLookup heap)) (List.tl stack)
	in Lazy_lists.ltake (n, lstack') @ Lists.drop n stack;;

let unwind state =
	let heap = getHeap state
	in let (a::ads) as stack = getStack state
	in let newState = function
		| NNum n -> (match getDump state with
			| (code', s')::d' ->
				let state' = putStack (a::s') (putDump d' state)
				in putCode code' state'
			| [] -> state (* G-machine has terminated *)
			)
		| NAppl(a1, a2) -> 
			putCode [Unwind] (putStack (a1::a::ads) state)
		| NGlobal(n, code) -> 
			let k = List.length ads
			in if k < n then (match getDump state with
				| [] -> raise (GmEvaluationError
				("Unwinding with too few arguments and empty dump"))
				| (code', s')::d' ->
					let state' = putStack (Lists.last stack :: s') state
					in putCode code' (putDump d' state')
			)
			else 
				let stack' = rearrange n heap stack
				in putCode code (putStack stack' state)
		| NInd ia -> putCode [Unwind] (putStack (ia::ads) state)
		| NConstr(tag, args) -> (match getDump state with
			| (code', s')::d' ->
			(* mind you, it's the same code as with NNum ! *)
				let state' = putStack (a::s') (putDump d' state)
				in putCode code' state'
			| [] -> 
				raise (GmEvaluationError ("Unwind with NConstr "
				^ "and empty dump - is it an error?"))
			)
	in newState (hLookup heap a);;

let rec allocNodes n heap = if n = 0 then (heap, []) else
	let (heap1, ads) = allocNodes (n - 1) heap
	in let (heap2, a) = hAlloc heap1 (NInd hNull)
	in (heap2, a::ads);;

let alloc n state =
	let (heap', ads) = allocNodes n (getHeap state)
	in let state' = putHeap heap' state
	in let stack = getStack state'
	in putStack (ads @ stack) state';;

let eval_instr state =
	let (a::ads) = getStack state
	in let code = getCode state
	in let s' = putDump ((code, ads)::getDump state) state
	in putCode [Unwind] (putStack [a] s');;

(*
let boxInteger n state =
	let (h', a) = hAlloc (getHeap state) (NNum n)
	in putStack (a::getStack state) (putHeap h' state);;

let boxBoolean b state =
	let bn = bool2gmBool b
	(*let b' = if b then 1 else 0*)(* 2 is tag for true; 1 for false *)
	in let (h', a) = hAlloc (getHeap state) bn (*(NNum b')*)
	in putStack (a::getStack state) (putHeap h' state);;
	(*let globals = getGlobals state
	in if b then
		let Some ta = Lists.aLookup globals "true"
		in putStack (ta::getStack state)
	else
		let Some fa = Lists.aLookup globals "false"
		in putStack (fa::getStack state) *)

let unboxInteger a state =
	let ub = function
		| NNum i -> i
		| _ -> raise (GmEvaluationError ("Unboxing a non-integer"))
	in ub (hLookup (getHeap state) a);;
*)
(* primitive1 :
	(b -> gmState -> gmState) (* boxing function *)
	-> (addr -> gmState -> a) (* unboxing function *)
	-> (a -> b) (* unary operator *)
	-> (gmState -> gmState) (* state transition *)
*)
let bool2int b = if b then 2 else 1;;
let int2bool n = if n = 2 then true
	else if n = 1 then false
	else raise (GmEvaluationError ("int other than 1 and 2 "
	^ "used as a boolean"));;

let primitive1 box unbox op state =
	let (n::ns) = getVStack state
	in putVStack (box (op (unbox n)) :: ns) state;;

let primitive2 box unbox op state =
	let (n0::n1::ns) = getVStack state
	in putVStack (box (op (unbox n0) (unbox n1)) :: ns) state;;

let arithmetic1 = primitive1 id id;;

let arithmetic2 = primitive2 id id;;

let comparison = primitive2 bool2int id;;

let logical1 = primitive1 bool2int int2bool;;

let logical2 = primitive2 bool2int int2bool;;

let dispatchArith1 = function
	| Neg -> arithmetic1 (fun x -> -x)
	| _ -> raise (GmEvaluationError (
		"don't know other unary arith operators other than Neg"))
	;;

let dispatchArith2 = function
	| Add -> arithmetic2 (+)
	| Sub -> arithmetic2 (-)
	| Mul -> arithmetic2 ( * )
	| Div -> arithmetic2 (/)
	| _ -> raise (GmEvaluationError (
		"don't know other binary arith operators other than: "
		^ "Add, Sub, Mul, Div"))
	;;

let dispatchComparison = function
	| Eq -> comparison (=)
	| Ne -> comparison (fun x y -> not (x = y))
	| Lt -> comparison (<)
	| Le -> comparison (<=)
	| Gt -> comparison (>)
	| Ge -> comparison (>=)
	| _ -> raise (GmEvaluationError (
		"don't know other binary comparison operators other than: "
		^ "Eq, Ne, Lt, Le, Gt, Ge"))
	;;

let dispatchLog1 = function
	| Not -> logical1 not
	| _ -> raise (GmEvaluationError (
		"don't know other unary logical operators other than Not"))
	;;

let dispatchLog2 = function
	| Or -> logical2 (||)
	| And -> logical2 (&&)
	| _ -> raise (GmEvaluationError (
		"don't know other binary logical operators other than: "
		^ "Or, And"))
	;;

let cond code1 code2 state =
	let (n::ns) = getVStack state
	in let state' = putVStack ns state
	in if n = gmVTrue then
		putCode (code1 @ getCode state') state'
	else if n = gmVFalse then
		putCode (code2 @ getCode state') state'
	else raise (GmEvaluationError ("Cond didn't find 2 or 1 "
		^ "on top of VStack"));;

let pack tag arity state =
	let stack = getStack state
	in if arity > List.length stack then
		raise (GmEvaluationError ("Pack needed " ^ string_of_int arity
		^ " arguments but found less"))
	else
		let args = Lists.take arity stack
		in let stack' = Lists.drop arity stack
		in let (heap', a) = hAlloc (getHeap state) (NConstr(tag, args))
		in putStack (a::stack') (putHeap heap' state)
	;;

let casejump alts state =
	let (a::ads) = getStack state
	in match hLookup (getHeap state) a with
		| NConstr(tag, args) -> (match Lists.aLookup alts tag with
			| Some code ->
				putCode (code @ getCode state) state
			| None -> raise (GmEvaluationError ("Casejump didn't "
			^ "find a tag " ^ string_of_int tag))
		)
		| _ -> raise (GmEvaluationError ("Casejump expected "
		^ "NConstr node on top of stack but found sth else"))
	;;

let split n state =
	let (a::ads) = getStack state
	in match hLookup (getHeap state) a with
		| NConstr(tag, args) ->
			putStack (args @ ads) state
		| _ -> raise (GmEvaluationError ("Split expected "
		^ "NConstr node on top of stack but found sth else"))
	;;

let print state =
	let (a::ads) = getStack state
	in match hLookup (getHeap state) a with
		| NNum n -> 
			let nout = getOutput state ^ string_of_int n ^ " "
			in putStack ads (putOutput nout state)
		| NConstr(tag, args) -> (* for now prints only arguments *)
			let n = List.length args
			in let evpr = List.flatten <| Lists.buildn n [Eval; Print]
			in let s' = putCode
				(evpr @ [PrintEndStruct] @ getCode state) state
			in let stag = string_of_int tag
			in putStack (args @ ads)
				(putOutput (getOutput s' ^ "[" ^ stag ^ ": ") s')
		| _ -> raise (GmEvaluationError ("Print expected "
		^ "NConstr or NNum node on top of stack but found sth else"))
	;;

let printEndStruct state =
	let nout = getOutput state ^ "] "
	in putOutput nout state;;

let dispatch i = match i with
	| Pushglobal f -> pushglobal f
	| Pushint n -> pushint n
	| Pushbasic n -> pushbasic n
	| Push n -> push n
	| Mkbool -> mkbool
	| Mkint -> mkint
	| S2V -> s2v
	| MkAppl -> mkAppl
	| Slide n -> slide n
	| Update n -> update n
	| Pop n -> pop n
	| Unwind -> unwind
	| Alloc n -> alloc n
	| Eval -> eval_instr
	| ar1 when List.mem ar1 [Neg] -> dispatchArith1 ar1
	| ar2 when List.mem ar2 [Add; Sub; Mul; Div] -> 
		dispatchArith2 ar2
	| cp2 when List.mem cp2 [Eq; Ne; Lt; Le; Gt; Ge] ->
		dispatchComparison cp2
	| log1 when List.mem log1 [Not] -> dispatchLog1 log1
	| log2 when List.mem log2 [And; Or] ->
		dispatchLog2 log2
	| Cond(c1, c2) -> cond c1 c2
	| Pack(tag, arity) -> pack tag arity
	| Casejump alts -> casejump alts
	| Split n -> split n
	| Print -> print
	| PrintEndStruct -> printEndStruct
	;;

let step state =
	let (i::is) = getCode state
	in dispatch i (putCode is state);;

let doAdmin s = putStats (statIncSteps (getStats s)) s;;

(* eval : gmState -> gmState list *)
let rec eval state =
	let rest_states = 
		if gmFinal state then []
		else eval (doAdmin (step state))
	in state::rest_states;;

let rec eval_only_last state =
	if gmFinal state then state
	else eval_only_last (doAdmin (step state));;

