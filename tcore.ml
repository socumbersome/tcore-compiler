
open Tcore_types;;

let preludeScDefs =
    [Supercombinator ("id", ["x"], EVar "x");
     Supercombinator ("compose", ["f"; "g"; "x"],
        EAppl(
            (EVar "f"),
            (EAppl ((EVar "g"), (EVar "x")))
        )
     );
     Supercombinator ("twice", ["f"],
        EAppl(
            (EAppl ((EVar "compose"), (EVar "f"))),
            (EVar "f")
        )
     )];;

let preludeTyConsDefs =
	[TypeDefinition (("List", ["a"]),
		[("Nil", []); ("Cons", [TVar "a"; Type ("List", [])])])
	];;
