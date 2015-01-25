(* header section *)
{
	open Tcore_parser
	open Lexing
	open Printf

	let incr_linenum lexbuf =
		let pos = lexbuf.Lexing.lex_curr_p in
		lexbuf.Lexing.lex_curr_p <- 
		{
			pos with
			Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
			Lexing.pos_bol = pos.Lexing.pos_cnum;
		}
}

(* definitions section *)
let digit = ['0'-'9']
let num = digit+
let alpha = ['a'-'z''A'-'Z']
let varch = alpha | digit | '_'
let var = ['a'-'z'] varch*
let capvar = ['A'-'Z'] varch*
(*let pmop = ['+' '-']
let mdop = ['*' '/']
let relop = ">" | "<" | ">=" | "<=" | "==" | "!="*)


(* rules section *)
rule token = parse
	| num as x { NUMBER (int_of_string x) }
	| [' ' '\t']+ { token lexbuf }
	| '\n' { incr_linenum lexbuf; token lexbuf }
	| "--" { onelinecomment lexbuf }
	| "{-" { multicomment lexbuf }
	| '(' { LPAREN }
	| ')' { RPAREN }
	| "let" { LET }
	| "letrec" { LETREC }
	| "case" { CASE }
	| "in" { IN }
	| "of" { OF }
	| "if" { IF }
	| "then" { THEN }
	| "else" { ELSE }
(*	| "Pack" { CONSTRUCTOR } *)
	| '_' { UNDERSCORE }
	| "type" { TYPE }
	| "true" { TRUE }
	| "false" { FALSE }
	| capvar as capitalname { CAPITALNAME capitalname }
	| var as varname { VARIABLE varname }
	| ',' { COMMA }
	| '\'' { APOSTROPHE }
	| '&' { AND }
	| '|' { OR }
	| '+' { PLUS }
	| '-' { MINUS }
	| "neg" { NEG }
	| "not" { NOT }
	| '*' { MULTIPLY }
	| '/' { DIVIDE }
	| '<' { LT }
	| "<=" { LE }
	| "==" { EQ }
	| "!=" { NE }
	| ">=" { GE }
	| '>' { GT }
	| ';' { SEMICOLON }
	| '=' { ASSIGNMENT }
	| '\\' { LAMBDA }
	| '.' { LAMBDA_DOT }
	| "->" { ALT_ARROW }
	| '[' { LSQUARE_BRACKET }
	| ']' { RSQUARE_BRACKET }
	| '#' { HASH }
	| '^' { CARET }
	| '{' { LCURLY_BRACKET }
	| '}' { RCURLY_BRACKET }
	| _ as c {
		printf "Lexer error: Unrecognized character: %c\n" c;
		token lexbuf
		}
	| eof { EOF }
and onelinecomment = parse
	| '\n' { incr_linenum lexbuf; token lexbuf }
	| _ { onelinecomment lexbuf }
and multicomment = parse
	| "-}" { token lexbuf }
	| '\n' { incr_linenum lexbuf; multicomment lexbuf }
	| _ { multicomment lexbuf }

(* trailer section *)
{ }
