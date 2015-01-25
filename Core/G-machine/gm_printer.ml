
open Gm_types;;
open Core_printer;;
open Heap;;
open Miscellaneous;;

let rec showInstruction = function
	| Unwind -> iStr "Unwind"
	| Pushglobal f -> iConcat [ iStr "Pushglobal "; iStr f ]
	| Push n -> iConcat [ iStr "Push "; iNum n ]
	| Pushint n -> iConcat [ iStr "Pushint "; iNum n ]
	| Pushbasic n -> iConcat [ iStr "Pushbasic "; iNum n ]
	| Mkbool -> iStr "Mkbool"
	| Mkint -> iStr "Mkint"
	| S2V -> iStr "S2V"
	| MkAppl -> iStr "MkAppl"
	| Slide n -> iConcat [ iStr "Slide "; iNum n ]
	| Update n -> iConcat [ iStr "Update "; iNum n ]
	| Pop n -> iConcat [ iStr "Pop "; iNum n ]
	| Alloc n -> iConcat [ iStr "Alloc "; iNum n ]
	| Eval -> iStr "Eval"
	| Add -> iStr "Add"
	| Sub -> iStr "Sub"
	| Mul -> iStr "Mul"
	| Div -> iStr "Div"
	| Neg -> iStr "Neg"
	| Eq -> iStr "Eq"
	| Ne -> iStr "Ne"
	| Lt -> iStr "Lt"
	| Le -> iStr "Le"
	| Gt -> iStr "Gt"
	| Ge -> iStr "Ge"
	| Or -> iStr "Or"
	| And -> iStr "And"
	| Not -> iStr "Not"
	| Cond(code1, code2) -> iConcat [ iStr "(Cond"; iNewline;
		iStr "1 -> ";
		shortShowInstructions 3 code1; iNewline; iStr "0 -> ";
		shortShowInstructions 3 code2; iStr ")" ]
	| Pack(tag, arity) -> iConcat [ iStr "Pack ";
		iNum tag; iStr ", "; iNum arity ]
	| Casejump alts -> iConcat [ iStr "(Casejump"; iNewline;
		iInterleave iNewline (
			List.map (fun (t, code) ->
				iConcat [ iNum t; iStr " -> "; 
				shortShowInstructions 4 code ] )
			alts
		); iStr ")" ]
	| Split n -> iConcat [ iStr "Split "; iNum n ]
	| Print -> iStr "Print"
	| PrintEndStruct -> iStr "PrintEndStruct"

and showInstructions code =
	iConcat [ iStr " Code:{";
		iIndent (iInterleave iNewline
			(List.map showInstruction code));
		iStr "}"; iNewline ]

and shortShowInstructions number code =
	let codes = List.map showInstruction (Lists.take number code)
	in let dotcodes = if List.length code > number then
		codes @ [ iStr "..." ]
		else codes
	in iConcat [ iStr "{"; iInterleave (iStr "; ") dotcodes; iStr "}"]
	;;

let showSC s (name, addr) =
	let NGlobal(arity, code) = hLookup (getHeap s) addr
	in iConcat [ iStr "Code for "; iStr name; iNewline;
		showInstructions code; iNewline; iNewline ]
	;;

let showNode s a = function
	| NNum n -> iNum n
	| NGlobal(n, g) -> 
		let v = (List.hd << List.map fst) (List.filter 
			(fun (_, b) -> a = b)
			(getGlobals s)
		)
		in iConcat [ iStr "Global "; iStr v]
	| NAppl(a1, a2) -> iConcat [ iStr "Appl "; iStr (showaddr a1);
		iStr " "; iStr (showaddr a2) ]
	| NInd a -> iConcat [ iStr "Ind "; iStr (showaddr a) ]
	| NConstr(t, ads) -> iConcat [ iStr "Constr "; iNum t;
		iStr " ["; iInterleave (iStr ", ")
			(List.map (iStr << showaddr) ads);
		iStr "]" ]
	;;

let showStackItem s a =
	iConcat [ iStr (showaddr a); iStr ": ";
		showNode s a (hLookup (getHeap s) a) ];;

let showStack s =
	iConcat [ iStr " Stack:["; iIndent (
		iInterleave iNewline
			(List.map (showStackItem s) (List.rev (getStack s))));
		iStr "]" ];;

let shortShowStack stack =
	iConcat [ iStr "["; iInterleave (iStr ", ")
		(List.map (iStr << showaddr) stack); iStr "]" ];;

let showDumpItem (code, stack) =
	iConcat [ iStr "<"; shortShowInstructions 3 code;
		iStr ", "; shortShowStack stack; iStr ">" ];;

let showDump s =
	iConcat [iStr " Dump:["; iIndent (
		iInterleave iNewline
			(List.map showDumpItem (List.rev (getDump s))));
		iStr "]" ];;

let showVStack s =
	iConcat [ iStr "VStack:["; iInterleave (iStr ", ")
		(List.map iNum (getVStack s));
	iStr "]" ];;

let showOutput s =
	iConcat [ iStr "Output:\""; iStr (getOutput s); iStr "\"" ];;

let showState s = iConcat [
	showOutput s; iNewline;
	showStack s; iNewline;
	showDump s; iNewline;
	showVStack s; iNewline;
	showInstructions (getCode s); iNewline ];;

let showStats s = iConcat [ iStr "Steps taken = "; 
	iNum (statGetSteps (getStats s)) ];;

let showResults states = 
	let s::ss = states
	in iDisplay (iConcat [
		iStr "Supercombinator definitions"; iNewline;
		iInterleave iNewline (List.map (showSC s) (getGlobals s));
		iNewline; iNewline; iStr "State transitions"; iNewline;
		iNewline; iLayn (List.map showState states); iNewline;
		iNewline; showStats (Lists.last states) ]
	);;

let showResult state =
	 iDisplay (iConcat [
		iLayn (List.map showState [state]); iNewline;
		iNewline; showStats (Lists.last [state]) ]
	);;
