all:
	@dune build

clean:
	@dune clean

install:
	@dune install

doc: satex.pdf

test: all
	$(MAKE) -C test

ci:
	git ci . -m "Worked on satex."
	git push

satex.pdf: README.md fig
	@$(MAKE) -C fig
	pandoc -V title:"SaTeX" -V author:"Samuel Mimram" -N --toc -V papersize:a4 $< -o $@
