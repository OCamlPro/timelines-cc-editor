DATABASE=ocptl_db
DBUPDATER=dbupdater

.PHONY: all build init db clean mr-proper

all: build

include libs/ez-pgocaml/libs/ez-pgocaml/Makefile.ezpg

build: parser db api js

init:
	bash build-deps.sh
	ocp-build init

db: db-update
	PGDATABASE=$(DATABASE) ocp-build csv-dbparser
	cp _obuild/csv-dbparser/csv-dbparser.asm csv-dbparser
parser:
	ocp-build csv-parser
	cp _obuild/csv-parser/csv-parser.asm csv-parser

api: db
	PGDATABASE=$(DATABASE) ocp-build api-lib
	ocp-build api
	cp _obuild/api/api.asm api

js:
	ocp-build ocptimeline-js
	cp _obuild/ocptimeline-js/ocptimeline-js.js www/js

clean:
	rm -rf _obuild/*
	rm -f -f www/js/ocptimeline-js.js
	rm -f csv-parser
	rm -f csv-dbparser
	rm -f db-version.txt

mr-proper: clean
	dropdb $(DATABASE)
