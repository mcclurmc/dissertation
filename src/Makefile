
# Add any included tex files here:
INCLUDED_TEX=intro.tex background.tex application.tex reflection.tex
MAIN_TEX=main.tex

OUTS=main.pdf # proposal.pdf

LATEXMK_OPTS=-f -bibtex -silent -pdf

all: $(OUTS)

main.pdf: $(MAIN_TEX) $(INCLUDED_TEX) bibliography.bib

proposal.pdf: bibliography.bib

%.pdf: %.tex
	latexmk $(LATEXMK_OPTS) $<

pvc:
	latexmk $(LATEXMK_OPTS) -pvc $(MAIN_TEX)

.PHONY: spellcheck
spellcheck:
	aspell -d en_GB -p aspell.en.pws -c application.tex
	aspell -d en_GB -p aspell.en.pws -c background.tex
	aspell -d en_GB -p aspell.en.pws -c intro.tex
	aspell -d en_GB -p aspell.en.pws -c main.tex
	aspell -d en_GB -p aspell.en.pws -c proposal.tex
	aspell -d en_GB -p aspell.en.pws -c reflection.tex

.PHONY:
clean:
	latexmk -c

.PHONY:
clean-all:
	latexmk -C
	rm -f *.bbl
