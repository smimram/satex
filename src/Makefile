SOURCES = extlib.ml lang.ml parser.mly lexer.mll satex.ml
RESULT = satex
PACKS = unix
ANNOTATE = true
PREFIX = /usr

all: dnc

install:
	install satex $(PREFIX)/bin

test ci:
	$(MAKE) -C .. $@

conflicts:
	menhir --explain parser.mly
	@cat parser.conflicts
	@rm -f parser.conflicts

include OCamlMakefile
