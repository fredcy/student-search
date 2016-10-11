build: elm.js

elm.js: src/Main.elm
	elm make src/Main.elm --output=elm.js
