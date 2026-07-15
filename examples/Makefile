all: examples.pdf

clean:
	latexmk -pdf -c examples.tex
	rm -f examples.pdf

%.pdf: %.tex
	latexmk -pdf $<
