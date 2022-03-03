all: build

build:
	@dune build

test ci:
	$(MAKE) -C .. $@

conflicts:
	menhir --explain parser.mly
	@cat parser.conflicts
	@rm -f parser.conflicts
