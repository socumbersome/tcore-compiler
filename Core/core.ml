(**
An example program:

main = double 21;
double x = x + x

will be represented as:

[("main", [], (EAppl (EVar "double") (ENum 21)));
 ("double", ["x"], (EAppl (EAppl (EVar "+") (EVar "x")) (EVar "x")))
]
**)

open Core_types;;

let bindersOf defs = List.map fst defs;;
let rhssOf defs = List.map snd defs;;

let isAtomicExpr e = match e with
    | EVar _ -> true
    | ENum _ -> true
    | _ -> false;;

(*
Standard functions defined in the prelude:
id x = x
(* K x y = x
K1 x y = y
S f g x = f x (g x) *)
compose f g x = f (g x)
twice f = compose f f
**)
let preludeDefs =
    [("id", ["x"], EVar "x");
 (*    ("K", ["x"; "y"], EVar "x");
     ("K1", ["x"; "y"], EVar "y");
     ("S", ["f"; "g"; "x"], 
        EAppl(
            (EAppl ((EVar "f"), (EVar "x"))),
            (EAppl ((EVar "g"), (EVar "x")))
        )
     ); *)
     ("compose", ["f"; "g"; "x"],
        EAppl(
            (EVar "f"),
            (EAppl ((EVar "g"), (EVar "x")))
        )
     );
     ("twice", ["f"],
        EAppl(
            (EAppl ((EVar "compose"), (EVar "f"))),
            (EVar "f")
        )
     );
(*	 ("cons", [], EConstr(2, 2));
	 ("nil", [], EConstr(1, 0)) *)
    ];;


