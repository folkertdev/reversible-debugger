all: pandoc
	rm -f paper.pdf
	@latexmk -pdf paper.tex

pandoc:
	pandoc -R --biblio paper.bib --csl ieee.csl -N -o paper.tex --template=llncs mapping.md 

web-pandoc:
	pandoc --webtex https://latex.codecogs.com/svg.latex?  -o paper.html paper.tex

install:
	mkdir -p ~/.pandoc/templates
	cp llncs.latex ~/.pandoc/templates

view: all
	mupdf paper.pdf

clean:
	rm -f paper.tex
	latexmk -c paper.tex

filewatch: all
	ls *.md | entr make

mymain:
	pdflatex --jobname=tfp-draft-submission mymain.tex 
	bibtex mymain 
	bibtex tfp-draft-submission
	pdflatex --jobname=tfp-draft-submission mymain.tex 

