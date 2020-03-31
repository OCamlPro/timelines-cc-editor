include libs/ez-pgocaml/libs/ez-pgocaml/Makefile.ezpg
DATABASE=ocptl_db
DBUPDATER=dbupdater

.PHONY: all build init db clean mr-proper

all: build

build:
	ocp-build init
	PGDATABASE=$(DATABASE) ocp-build
	cp _obuild/csv-parser/csv-parser.asm csv-parser
	cp _obuild/ocptimeline-js/ocptimeline-js.js www/js

init:
	git submodule init
	git submodule update
	cd libs/ocplib-jsutils
	make
	cd ../ez-pgocaml
	make
	cd ../..

db: db-update

clean:
	rm -rf _obuild/*
	rm www/js/ocptimeline-js.js
	rm csv-parser

mr-proper: clean
	dropdb $(DATABASE)
