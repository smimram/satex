SOURCES = extlib.ml lang.ml parser.mly lexer.mll satex.ml
RESULT = satex
PACKS = graphics unix
ANNOTATE = true

all: dnc
	OCAMLRUNPARAM=b ./satex ../test/test.satex

ci:
	$(MAKE) -C .. $@

conflicts:
	menhir --explain parser.mly
	@cat parser.conflicts
	@rm -f parser.conflicts

include OCamlMakefile
