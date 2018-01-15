all:
	export PORT=8000
	cd frontend && elm-make src/Main.elm --output index.html && cd ..
	stack build reversible-debugger:server
	stack exec server
