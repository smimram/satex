TEX = $(filter-out test.tex,$(wildcard *.tex))
PDF = $(TEX:.tex=.pdf)
SATEX = dune exec ../src/satex.exe

all: $(PDF) test.pdf

test.pdf: test.tex
	pdflatex test.tex > /dev/null
	cat test.satex
	$(SATEX) test.satex
	-cat test.satix
	pdflatex test.tex > /dev/null

clean:
	rm -f *.aux *.log
	rm -f *.satex *.satix
	rm -f $(PDF)

ci:
	$(MAKE) -C .. $@

%.satex: %.tex
	pdflatex $<

%.satix: %.satex
	$(SATEX) $<

%.pdf: %.tex %.satix
	pdflatex $<
