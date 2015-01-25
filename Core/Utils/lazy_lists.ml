type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;
exception EmptyLazyList;;

let lhd = function
	| LNil -> raise EmptyLazyList
	| LCons(x, _) -> x;;

let ltl = function
	| LNil -> raise EmptyLazyList
	| LCons(_, lazy t) -> t;;

let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;

let rec ltake = function
	| (0, _) -> []
	| (_, LNil) -> []
	| (n, LCons(x, lazy xs)) -> x::ltake(n - 1, xs);;

let rec lmap f = function
	| LNil -> LNil
	| LCons(x, lazy xs) -> LCons(f x, lazy(lmap f xs));;

let rec lmapAccuml f acc = function
	| LNil -> (acc, LNil)
	| LCons(x, lazy xs) -> let (acc1, x') = f acc x
		in let (acc2, xs') = lmapAccuml f acc1 xs
		in (acc2, LCons(x', lazy xs'));;

let rec tolazy = function
	| [] -> LNil
	| x::xs -> LCons(x, lazy(tolazy xs));;

let rec lzip l1 l2 = match (l1, l2) with
	| (LNil, _) -> LNil
	| (_, LNil) -> LNil
	| (LCons(x, lazy xs), LCons(y, lazy ys)) -> 
		LCons((x, y), lazy(lzip xs ys));;

let rec ldrop n xs = match (n, xs) with
	| (0, _) -> xs
	| (_, LNil) -> LNil
	| (n, LCons(_, lazy ys)) -> ldrop (n - 1) ys;;

let rec fromlazy = function
	| LNil -> []
	| LCons(x, lazy xs) -> x::(fromlazy xs);;
