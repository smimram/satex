FIG = $(wildcard *.tex)
PDF = $(FIG:.tex=.pdf)
SVG = $(PDF:.pdf=.svg)
SATEX = ../../src/satex

all: pdf

pdf: $(PDF)

svg: $(SVG)

%.satex: %.tex
	pdflatex $<

%.satix: %.satex
	$(SATEX) $<

%.pdf: %.tex %.satix
	pdflatex $<

%.svg: %.pdf
	inkscape $< -o $@
