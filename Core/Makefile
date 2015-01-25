RESULT = core_compiler_interact
SOURCES = \
	./Utils/miscellaneous.ml ./Utils/lists.ml \
	./Utils/lazy_lists.ml ./Utils/heap.ml \
	./Utils/unique_names.ml \
	\
	core_types.ml core.ml \
	core_AST_utils.ml core_case_lifter.ml \
	core_pack_lifter.ml \
	core_lambda_lifter.ml \
	core_parser.mly core_lexer.mll \
	core_printer.ml \
	./G-machine/gm_types.ml ./G-machine/gm_printer.ml \
	./G-machine/gm_compiler.ml \
	./G-machine/gm_evaluator.ml  \
	core_compiler.ml \
	core_compiler_interact.ml

OCAMLMAKEFILE = OCamlMakefile
LIBS = str
include $(OCAMLMAKEFILE)
