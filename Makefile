all:
	ocp-build

init:
	git sobmodule init
	git submodule update

install:
	cp data.json www
	cp _obuild/ocptimeline-js/ocptimeline-js.js
