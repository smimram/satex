SOURCES = lang.ml parser.mly lexer.mll satex.ml
RESULT = satex
PACKS = graphics
ANNOTATE = true

all: dnc
	OCAMLRUNPARAM=b ./satex ../test/test.satex

ci:
	git ci . -m "Worked on satex."
	git push

conflicts:
	menhir --explain parser.mly
	@cat parser.conflicts
	@rm -f parser.conflicts

include OCamlMakefile
