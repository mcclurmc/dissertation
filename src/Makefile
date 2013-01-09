
PDF=proposal.pdf

all: $(PDF)

%.pdf:	%.tex bibliography.bib
	latexmk -f -bibtex -silent -pdf $<

PHONY: spellcheck

spellcheck:
	aspell -c proposal.tex -d en_GB

clean:
	latexmk -c

clean-all:
	latexmk -C


