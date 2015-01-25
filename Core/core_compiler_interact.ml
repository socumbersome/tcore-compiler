open Miscellaneous;;
open Core_compiler;;

let usage = "---- Core compiler usage:\n" ^
		"if compiler's name is `cc`, then the correct invokation\n" ^
		"has the form: cc s n\n" ^
		"where `s` is a path to a TCore source file and\n`n` is a number " ^
		"from 1 to 3 with the following meaning:\n" ^
		"- 1 - print lifted Core program (is not a Core source program)\n" ^
		"- 2 - run program on G-machine and print all intermediate states\n" ^
		"- 3 - run program on G-machine and print the final state only\n"
	;;

let main () =
	if Array.length (Sys.argv) < 3 then print_string usage else
	let cin = open_in Sys.argv.(1)
	in let lexbuf = Lexing.from_channel cin
	in let program = Core_parser.program Core_lexer.token lexbuf
	in (match Sys.argv.(2) with
		| "1" -> printLifted program
		| "2" -> toGmAndPrintAll program
		| "3" -> toGmAndPrintLast program
		| _ -> print_string usage
	);;

let _ = main ();;
