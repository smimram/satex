SOURCES = lang.ml parser.mly lexer.mll satex.ml
RESULT = satex
ANNOTATE = true

all: dnc
	OCAMLRUNPARAM=b ./satex ../test/test.satex

include OCamlMakefile
