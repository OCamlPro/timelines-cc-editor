DATABASE=ocptl_db
DBUPDATER=dbupdater

.PHONY: all build init db clean mr-proper

all: build

include libs/ez-pgocaml/libs/ez-pgocaml/Makefile.ezpg

build:
	PGDATABASE=$(DATABASE) ocp-build
	cp _obuild/csv-parser/csv-parser.asm csv-parser
	cp _obuild/csv-dbparser/csv-dbparser.asm csv-dbparser
	cp _obuild/ocptimeline-js/ocptimeline-js.js www/js

init:
	git submodule init
	git submodule update
	cd libs/ocplib-jsutils
	make
	cd ../ez-pgocaml
	make
	cd ../..
	ocp-build init

db: db-update

parser:
	ocp-build csv-parser
	cp _obuild/csv-parser/csv-parser.asm csv-parser

clean:
	rm -rf _obuild/*
	rm www/js/ocptimeline-js.js
	rm csv-parser

mr-proper: clean
	dropdb $(DATABASE)
