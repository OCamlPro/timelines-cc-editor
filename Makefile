include libs/ez-pgocaml/libs/ez-pgocaml/Makefile.ezpg
DATABASE=ocptl_db
DBUPDATER=dbupdater

all:
	PGDATABASE=$(DATABASE) ocp-build

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

install:
	cp data.json www
	cp _obuild/ocptimeline-js/ocptimeline-js.js www/js
