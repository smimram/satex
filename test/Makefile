SATEX = ../src/satex

all:
	pdflatex test.tex > /dev/null
	cat test.satex
	$(SATEX) test.satex
	-cat test.satix
	pdflatex test.tex > /dev/null

