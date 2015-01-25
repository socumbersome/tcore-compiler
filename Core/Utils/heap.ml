(** A heap of t is a collection of objects of type t,
each identified by a unique address of type addr
*)

open Lazy_lists;;

type addr = int;;

(** heap is represented as a triple, containing, in order:
- the number of objects in the heap
- a list of unused addresses
- an association list mapping addresses to objects
*)
type 'a heap = int * int llist * (int * 'a) list;;

exception AddressNotFound of string;;

let hInitial = (0, Lazy_lists.lfrom 1, []);;

(* hAlloc : 'a heap -> a -> ('a heap, addr) *)
let hAlloc (size, LCons(next, lazy free), a2o) obj =
	((size + 1, free, (next, obj)::a2o), next);;

(* hUpdate : 'a heap -> addr -> a -> 'a heap *)
let hUpdate ((size, free, a2o) : 'a heap) a obj : 'a heap =
	(size, free, (a, obj)::(List.remove_assoc a a2o));;

let hNull = 0;;
let hIsNull a = a == 0;;
let showaddr a = "#" ^ (string_of_int a);;

let rec rmaddr a2o a = match a2o with
	| [] -> raise (AddressNotFound ("Attempt to update or free " ^
		"a nonexistent address: " ^ (showaddr a)))
	| ((a1, n) as h)::t -> if a1 = a then t
		else h::(rmaddr t a);;

let hFree (size, free, a2o) a : 'a heap =
	(size - 1, LCons(a, lazy free), rmaddr a2o a);;

let hLookup (_, _, a2o) a =
	try List.assoc a a2o with
		| Not_found -> raise (AddressNotFound (showaddr a));;

let hAddresses (_, _, a2o) = List.map fst a2o;;

let hSize (size, _, _) = size;;


