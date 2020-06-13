SOURCES = extlib.ml lang.ml parser.mly lexer.mll satex.ml
RESULT = satex
PACKS = unix
ANNOTATE = true

all: dnc

test ci:
	$(MAKE) -C .. $@

conflicts:
	menhir --explain parser.mly
	@cat parser.conflicts
	@rm -f parser.conflicts

include OCamlMakefile
