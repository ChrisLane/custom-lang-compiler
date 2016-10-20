default: main

main: main.native

test:
	bash tests/runtests.sh

clean:
	rm -rf _build

%.native:
	ocamlbuild -use-ocamlfind $@
	mv $@ $*

.PHONY: default test clean
