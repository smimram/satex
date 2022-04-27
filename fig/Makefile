FIG = $(wildcard *.tex)
PDF = $(FIG:.tex=.pdf)
SVG = $(PDF:.pdf=.svg)
SATEX = dune exec ../src/satex.exe

all: svg

pdf: $(PDF)

svg: $(SVG)

add:
	git add *.tex *.svg

clean:
	rm -f $(PDF) $(SVG)

%.satex: %.tex
	pdflatex $<

%.satix: %.satex
	$(SATEX) $<

%.pdf: %.tex %.satix
	pdflatex $<

%.svg: %.pdf
	# inkscape --pdf-poppler $< -o $@
	pdftocairo -svg -f 1 -l 1 $< $@
	rsvg-convert -f svg --zoom=2 $@ -o $@
