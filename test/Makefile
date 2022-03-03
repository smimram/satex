SATEX = dune exec ../src/satex.exe

all: 2groups.pdf bug1.pdf
	pdflatex test.tex > /dev/null
	cat test.satex
	$(SATEX) test.satex
	-cat test.satix
	pdflatex test.tex > /dev/null

clean:
	rm -f *.satex *.satix

ci:
	$(MAKE) -C .. $@

%.satex: %.tex
	pdflatex $<

%.satix: %.satex
	$(SATEX) $<

%.pdf: %.tex %.satix
	pdflatex $<
