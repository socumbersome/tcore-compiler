
open Core_types;;
open Heap;;

exception GmCompilationError of string;;
exception GmEvaluationError of string;;

type instruction
	= Unwind
	| Pushglobal of cName
	| Pushint of int
	| Pushbasic of int
	| Mkbool
	| Mkint
	| S2V (* take from Stack and put on VStack - named Get in paper *)
	| Push of int
	| MkAppl
	| Slide of int
	| Update of int
	| Pop of int
	| Alloc of int
	| Eval	(* evaluates item on top of stack to WHNF *)
	| Add | Sub | Mul | Div | Neg
	| Eq | Ne | Lt | Le | Gt | Ge
	| Or | And | Not
	| Cond of gmCode * gmCode
	| Pack of int * int
	| Casejump of (int * gmCode) list
	| Split of int
	| Print
	| PrintEndStruct

and  gmCode = instruction list;;
type gmStack = addr list;;

type gmDumpItem = gmCode * gmStack;;
type gmDump = gmDumpItem list;;

type node
	= NNum of int
	| NAppl of addr * addr	(* applies the function at the
		first address to the expression at the second address *)
	| NGlobal of int * gmCode (* number of agruments and
		a code sequence to be executed when arguments are given *)
	| NInd of addr (* an indirection node *)
	| NConstr of int * addr list (* structured data - int is a tag *)
	;;

let gmVTrue = 2;;
let gmVFalse = 1;;

let gmPackRegexp = Str.regexp 
	"^Pack{\\([1-9][0-9]*\\),\\([0-9]+\\)}$";;

type gmVStack = int list;;

type gmHeap = node heap;;

type gmGlobals = (cName, addr) Lists.assoc;;

type gmStats = int;;

type gmOutput = string;;

type gmState = (
	gmOutput *	(* current output *)
	gmCode *	(* current instruction stream *)
	gmStack *	(* current stack *)
	gmDump *	(* current Dump *)
	gmVStack *	(* V-stack - for keeping intermediate results *)
	gmHeap *	(* heap of nodes *)
	gmGlobals *	(* global addresses in the heap *)
	gmStats	(* statistics *)
	);;

let getOutput (out, _, _, _, _, _, _, _) = out;;
let putOutput out (_, code, stack, dump, vstack, heap, globals, stats) =
	(out, code, stack, dump, vstack, heap, globals, stats);;

let getCode (_, code, _, _, _, _, _, _) = code;;
let putCode code (out, _, stack, dump, vstack, heap, globals, stats) =
	(out, code, stack, dump, vstack, heap, globals, stats);;

let getStack (_, _, stack, _, _, _, _, _) = stack;;
let putStack stack (out, code, _, dump, vstack, heap, globals, stats) =
	(out, code, stack, dump, vstack, heap, globals, stats);;

let getDump (_, _, _, dump, _, _, _, _) = dump;;
let putDump dump (out, code, stack, _, vstack, heap, globals, stats) =
	(out, code, stack, dump, vstack, heap, globals, stats);;

let getVStack (_, _, _, _, vstack, _, _, _) = vstack;;
let putVStack vstack (out, code, stack, dump, _, heap, globals, stats) =
	(out, code, stack, dump, vstack, heap, globals, stats);;

let getHeap (_, _, _, _, _, heap, _, _) = heap;;
let putHeap heap (out, code, stack, dump, vstack, _, globals, stats) =
	(out, code, stack, dump, vstack, heap, globals, stats);;

let getGlobals (_, _, _, _, _, _, globals, _) = globals;;
let putGlobals globals (out, code, stack, dump, vstack, heap, _, stats) =
	(out, code, stack, dump, vstack, heap, globals, stats);;

let statInitial = 0;;
let statIncSteps s = s + 1;;
let statGetSteps s = s;;

let getStats (_, _, _, _, _, _, _, stats) = stats;;
let putStats stats (out, code, stack, dump, vstack, heap, globals, _) =
	(out, code, stack, dump, vstack, heap, globals, stats);;
