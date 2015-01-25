# TCore compiler (a typed extension of Core language)

1. Building
2. Syntax
3. Caveats
4. Weird conventions
5. Known bugs
6. To do

## Building

Run `make` command (and make sure you have OCaml in version at least 4.02.1) and a `tcore_compiler` file should be created in the current directory
(and please don't be scared from the plethora of warnings :))

Usage:
correct invokation has the form: tcore_compiler s n
where `s` is a path to a TCore source file and `n` is a number
from 1 to 5 with the following meaning:

* 1 - print inferred types\n
* 2 - print raw Core program (is a Core source program)
* 3 - print lifted Core program (is not a Core source program)
* 4 - run program on G-machine and print all intermediate states
* 5 - run program on G-machine and print the final state only

## Syntax

Instead of presenting formal BNF grammar, please have a look at
a (hopefully) exhaustive example and you should quickly get the feel
of how to write TCore programs:

<pre>
type Tree a = Leaf | Node (Tree a) a (Tree a) ;

prod tree cont = case tree of
	Leaf -> cont 1 ,
	Node left n right ->
		if n == 0 then 0 else
		prod left
		(\ pl . prod right
			(\ pr . cont (pl * n * pr))
		)
	;

sum3 = \x y z. x + y + z ;

iseven n = letrec
	even = \n. if n == 0 then true else odd (n - 1) ;
	odd = \n. if n == 0 then false else even (n - 1)
	in even n
	;

map f xs = case xs of
	Nil -> Nil ,
	Cons y ys -> Cons (f y) (map f ys)
	;

main = let t = Node (Node Leaf 2 (Node Leaf 4 Leaf) ) 3 
	(Node (Node Leaf 1 Leaf) 5 Leaf )
	in prod t (\ x . x) == 120

</pre>

## Caveats

* In case expressions, one is allowed to use only patterns with
depth exactly 1

## Weird conventions

* Because Core is untyped and doesn't carry type information from TCore, its output has only two forms: either it's a number, or a structure with a tag, arity and list of contents that again can be numbers or structures.
* true value is represented as a structure [2: ], and false as [1: ]

## Known bugs

* Typechecker in TCore is insensitive for types in arguments in type constructors - it looks only at the number of arguments (for example, in `Node (Tree a) a (Tree a)`, one can put `Node 1 2 3` into code and typechecker won't notice the malice

## To do

* Fix bugs...
* Allow patterns of arbitrary length
* Implement a garbage collector
* Implement tail recursion

