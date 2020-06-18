PREFIX=/usr

all: satex.pdf
	$(MAKE) -C src

install:
	$(MAKE) -C src $@
	install -d statex.sty $(PREFIX)/share/texlive/texmf-dist/tex/latex/satex/

test: all
	$(MAKE) -C test

ci:
	git ci . -m "Worked on satex."
	git push

satex.pdf: README.md fig
	pandoc -V title:"SaTeX" -V author:"Samuel Mimram" -N --toc --default-image-extension=.pdf $< -o $@

fig:
	$(MAKE) -C fig
