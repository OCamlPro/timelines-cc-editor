DATABASE=ocptl_db
DBUPDATER=dbupdater

.PHONY: all build init db clean mr-proper

all: build

include libs/ez-pgocaml/libs/ez-pgocaml/Makefile.ezpg

build: parser db api js js-admin

init:
	git submodule init
	git submodule update
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

js-admin:
	ocp-build ocptimeline-js-admin
	cp _obuild/ocptimeline-js-admin/ocptimeline-js-admin.js www/js

clean:
	rm -rf _obuild/*
	rm -f -f www/js/ocptimeline-js.js
	rm -f csv-parser
	rm -f csv-dbparser

mr-proper: clean
	dropdb $(DATABASE)
