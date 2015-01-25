open Miscellaneous;;
open Tcore_typechecker;;
open Tcore;;

let printTypes nt = print_string (Tcore_printer.pprTypes nt);;

let usage = "---- TCore compiler usage:\n" ^
		"if compiler's name is `tc`, then the correct invokation\n" ^
		"has the form: tc s n\n" ^
		"where `s` is a path to a TCore source file and\n`n` is a number " ^
		"from 1 to 5 with the following meaning:\n" ^
		"- 1 - print inferred types\n" ^
		"- 2 - print raw Core program (is a Core source program)\n" ^
		"- 3 - print lifted Core program (is not a Core source program)\n" ^
		"- 4 - run program on G-machine and print all intermediate states\n" ^
		"- 5 - run program on G-machine and print the final state only\n"
	;;

let main () =
	if Array.length (Sys.argv) < 3 then print_string usage else
	let cin = open_in Sys.argv.(1)
	in let lexbuf = Lexing.from_channel cin
	in let program = Tcore_parser.program Tcore_lexer.token lexbuf
	in let (prog', tc2i, i2t) = Tcore_tconstr_transformer.transform
		(preludeTyConsDefs @ preludeScDefs @ program)
	in let nt = Tcore_typechecker.typecheck tc2i i2t prog'
	in if Sys.argv.(2) = "1" then printTypes nt else
	let coreprog = Tcore2core.transform prog'
	in (match Sys.argv.(2) with
		| "2" -> Core_compiler.printCore coreprog
		| "3" -> Core_compiler.printLifted coreprog
		| "4" -> Core_compiler.toGmAndPrintAll coreprog
		| "5" -> Core_compiler.toGmAndPrintLast coreprog
		| _ -> print_string usage
		)
	;;
	(*printTypes nt;;*)
	(*	typecheckOnly <| program;;*)

let _ = main ();;
