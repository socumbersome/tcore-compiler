RESULT = tcore_compiler
SOURCES = \
	./Core/Utils/miscellaneous.ml ./Core/Utils/lists.ml \
	./Core/Utils/lazy_lists.ml ./Core/Utils/heap.ml \
	./Core/Utils/unique_names.ml \
	\
	./Core/core_types.ml ./Core/core.ml \
	./Core/core_AST_utils.ml ./Core/core_case_lifter.ml \
	./Core/core_pack_lifter.ml \
	./Core/core_lambda_lifter.ml \
	./Core/core_parser.mly ./Core/core_lexer.mll \
	./Core/core_printer.ml \
	./Core/G-machine/gm_types.ml ./Core/G-machine/gm_printer.ml \
	./Core/G-machine/gm_compiler.ml \
	./Core/G-machine/gm_evaluator.ml  \
	./Core/core_compiler.ml \
	\
	tcore_types.ml tcore.ml \
	tcore_tconstr_transformer.ml \
	tcore2core.ml \
	tcore_parser.mly tcore_lexer.mll \
	tcore_tctypes.ml tcore_printer.ml \
	tcore_typechecker.ml tcore_compiler.ml

OCAMLMAKEFILE = OCamlMakefile
LIBS = str
include $(OCAMLMAKEFILE)
