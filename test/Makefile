SATEX = ../src/satex

all:
	pdflatex test.tex
	cat test.satex
	$(SATEX) test.satex
	-cat test.satix
	pdflatex test.tex

