PREFIX=/usr

all:
	$(MAKE) -C src

doc: satex.pdf

install:
	$(MAKE) -C src $@
	install -d statex.sty $(PREFIX)/share/texlive/texmf-dist/tex/latex/satex/

test: all
	$(MAKE) -C test

ci:
	git ci . -m "Worked on satex."
	git push

satex.pdf: README.md fig
	@$(MAKE) -C fig
	pandoc -V title:"SaTeX" -V author:"Samuel Mimram" -N --toc -V papersize:a4 $< -o $@
