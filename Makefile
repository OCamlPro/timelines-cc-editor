DATABASE=ocptl_db
DBUPDATER=dbupdater

.PHONY: all build init db clean mr-proper

all: build

init:
	git submodule init
	git submodule update
	bash opam_build.sh
	bash build_deps.sh

build: website
	PGDATABASE=$(DATABASE) dune build src/db/dBUpdater.exe
	_build/default/src/db/dBUpdater.exe
	PGDATABASE=$(DATABASE) dune build
	cp -f _build/default/src/api/api.exe api
	cp -f _build/default/src/ui/ocptimeline_js.bc.runtime.js www/assets/js/ocptimeline-js.js

website:
	bash generate_website.sh

clean:
	rm -rf _build/*
	rm -f csv-parser
	rm -f csv-dbparser
	rm -f db-version.txt

mr-proper: clean
	opam switch remove . -y
	dropdb $(DATABASE)

