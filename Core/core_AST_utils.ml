
open Core_types;;

exception UnsaturatedConstructor;;

let dismantleConstr expr =
	let rec aux gathered howmany = function
		| EAppl(e, ei) -> aux (ei::gathered) (howmany + 1) e
		| EConstr(tag, arity) as cons -> if arity = howmany then
			(cons, gathered)
			else raise UnsaturatedConstructor
		| _ -> raise UnsaturatedConstructor
	in aux [] 0 expr;;

(* terribly inefficient ;(
TODO: find a better way! *)
let isSaturatedConstr expr = try 
	let _ = dismantleConstr expr in true with
		| UnsaturatedConstructor -> false
	;;
