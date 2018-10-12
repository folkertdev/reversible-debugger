all: pandoc biblio.bib
	rm -f paper.pdf
	@latexmk -pdf thesis.tex -halt-on-error

force: 
	rm -f paper.pdf
	pdflatex thesis.tex -halt-on-error

pandoc:
	pandoc --toc -R --biblio biblio.bib --csl ieee.csl -N -o thesis.tex --template=llncs thesis.md

filewatch:
	ls *.md *.bib | entr make 

scratchpad: 
	pandoc scratchpad.md -o scratchpad.tex
	cat scratchpad.tex
