all:
	ocp-build

init:
	git submodule init
	git submodule update
	cd libs/ocplib-jsutils
	make
	cd ../..

clean:
	rm -rf _obuild/*

install:
	cp data.json www
	cp _obuild/ocptimeline-js/ocptimeline-js.js www/js
