
open Core_printer;;
open Tcore_tctypes;;

let tvar2str (TV s) = s;;

let rec type2str t = match t with
	| TVar tv -> tvar2str tv
	| TCon name -> name
	| TArr(t1, t2) -> 
		let right = type2str t2
		in if isArrow t1 then "(" ^ type2str t1 ^ ") -> " ^ right
		else type2str t1 ^ " -> " ^ right
	;;

let scheme2str (Forall(xs, t)) =
	let t2s = type2str t in
	if xs = [] then t2s else
		"forall" ^ 
		(List.fold_left (fun a b -> a ^ " " ^ (tvar2str b)) "" xs)
		^ ". " ^ t2s;;

let pprTypes nts = iDisplay (iInterleave iNewline 
		(List.map (fun (n, t) -> 
			iConcat [iStr n; iStr " :: "; iStr t]
		) nts)
	);;

