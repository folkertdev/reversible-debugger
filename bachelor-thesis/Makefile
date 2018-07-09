all: pandoc
	rm -f paper.pdf
	@latexmk -pdf thesis.tex -halt-on-error

pandoc:
	pandoc --toc -R --biblio biblio.bib --csl ieee.csl -N -o thesis.tex --template=llncs thesis.md

filewatch:
	ls *.md | entr make 
