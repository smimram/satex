SATEX = ../src/satex

all: 2groups.pdf
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
