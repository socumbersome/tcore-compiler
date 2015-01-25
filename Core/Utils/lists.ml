type ('a, 'b) assoc = ('a * 'b) list;;

let rec aLookup l k = match l with
	| [] -> None
	| ((k', v)::bs) -> if k = k' then Some v else aLookup bs k;;

let rec aLookupWithDefault l k d = match aLookup l k with
	| None -> d
	| Some x -> x;;

let aDomain alist = List.map fst alist;;
let aRange alist = List.map snd alist;;

let aRmvKey k l = List.filter (fun (k', v') -> k <> k') l;;

let aMap f l = List.map (fun (k, v) -> (k, f v)) l;;

let aUpdate k v l = let l' = aRmvKey k l
	in (k, v)::l';;

let aEmpty = [];;

let aSingleton k v = [(k, v)];;

let aUnion l1 l2 = l1 @ (List.filter (fun (k, v) ->
		not (List.mem_assoc k l1)
	) l2);;

let rmfst xs a =
	let rec aux xs acc = match xs with
		| [] -> List.rev acc
		| y::ys -> if y = a then (List.rev acc) @ ys
			else aux ys (y::acc)
	in aux xs [];;

let rmall xs a = List.filter (fun x -> x <> a) xs;;

let union xs ys = xs @ (List.filter (fun y -> not (List.mem y xs)) ys);;

let diff xs ys = List.filter (fun x -> not (List.mem x ys)) xs;;

let foldr f b xs = List.fold_right f xs b;;

(** takes min(n, length of xs) elements from xs *)
let rec take n xs = match n with
	| 0 -> []
	| _ -> (match xs with
		| [] -> xs
		| y::ys -> y::(take (n - 1) ys)
	);;

let rec buildn n x = match n with
	| 0 -> []
	| _ -> x::(buildn (n - 1) x);;

(* mapAccuml: ('a -> 'b -> ('a, 'c)) -> 'a 
	-> 'b list -> ('a, 'c list) *)
let rec mapAccuml f acc = function
	| [] -> (acc, [])
	| x::xs -> let (acc1, x') = f acc x
		in let (acc2, xs') = mapAccuml f acc1 xs
		in (acc2, x'::xs');;

let rec zip l1 l2 = match (l1, l2) with
	| ([], _) -> []
	| (_, []) -> []
	| (x::xs, y::ys) -> (x, y)::(zip xs ys);;

let rec zip2t3 lp1 l2 = match (lp1, l2) with
	| ([], _) -> []
	| (_, []) -> []
	| ((x1, x2)::xs, y::ys) -> (x1, x2, y)::(zip2t3 xs ys);;

let rec zip3 l1 l2 l3 = match (l1, l2, l3) with
	| ([], _, _) -> []
	| (_, [], _) -> []
	| (_, _, []) -> []
	| (x::xs, y::ys, z::zs) -> (x, y, z)::(zip3 xs ys zs);;

let rec drop n xs = match (n, xs) with
	| (0, _) -> xs
	| (_, []) -> xs
	| (n, _::ys) -> drop (n - 1) ys;;
 
let rec last = function
	| [] -> failwith "No last element exists in empty list"
	| [x] -> x
	| x::xs -> last xs;;

let rec range a b = if a > b then [] else a::(range (a+1) b);;
