build: elm.js

elm.js: src/Main.elm
	elm make src/Main.elm --output=$@

###

dist: site site/elm.js site/index.html site/autocomplete.css

site:
	mkdir site

site/elm.js: elm.js
	cp $< $@

site/index.html: index.html
	perl -pe 's#{{ *service_url *}}#$(SERVICE_URL)#' index.html >$@

site/autocomplete.css: autocomplete.css
	cp $< $@
