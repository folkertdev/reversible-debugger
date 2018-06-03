all: 
	rm -f tfp-presentation.pdf
	@latexmk -pdf tfp-presentation.tex -halt-on-error

filewatch: all
	ls *.tex | entr make
