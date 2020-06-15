all:
	$(MAKE) -C src

install:
	$(MAKE) -C src $@
	install -d statex.sty $(PREFIX)/share/texlive/texmf-dist/tex/latex/satex/

test: all
	$(MAKE) -C test

ci:
	git ci . -m "Worked on satex."
	git push
