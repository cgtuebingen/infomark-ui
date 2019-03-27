
clean:
	rm -r elm-stuff/

build:
	elm make --output=elm.js src/Main.elm

release:
	./optimize.sh src/Main.elm

run:
	elm-live src/Main.elm -u -p 8081 -- --output=elm.js