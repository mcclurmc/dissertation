
PDF=proposal.pdf

all: $(PDF)

%.pdf:	%.tex
	latexmk -f -bibtex -silent -pdf $<

clean:
	latexmk -c

clean-all:
	latexmk -C


