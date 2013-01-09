
PDF=proposal.pdf

all: $(PDF)

%.pdf:	%.tex bibliography.bib
	latexmk -f -bibtex -silent -pdf $<

PHONY: spellcheck

spellcheck:
	aspell -d en_GB -p aspell.en.pws -c proposal.tex

clean:
	latexmk -c

clean-all:
	latexmk -C


