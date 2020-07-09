DATABASE=ocptl_db
DBUPDATER=dbupdater

.PHONY: all build init db clean mr-proper

all: build

init:
	git submodule init
	git submodule update
	bash opam_build.sh
	bash build_deps.sh

build: db-update js website
	PGDATABASE=$(DATABASE) dune build
	cp -f _build/default/src/api/api.exe api

db-update:
	PGDATABASE=$(DATABASE) dune build src/db/dBUpdater.exe
	_build/default/src/db/dBUpdater.exe
js:
	dune build src/ui/ocptimeline_js.bc.js --profile release
	cp -f _build/default/src/ui/ocptimeline_js.bc.js www/assets/js/ocptimeline-js.js

website:
	cp -f libs/vue-jsoo/js/* www/assets/js/ 
	bash generate_website.sh

clean:
	rm -rf _build/*
	rm -f csv-parser
	rm -f csv-dbparser
	rm -f db-version.txt

mr-proper: clean
	opam switch remove . -y
	dropdb $(DATABASE)

