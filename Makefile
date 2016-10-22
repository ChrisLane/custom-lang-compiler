default: main

main: main.native

test: main clearscr runtests

runtests:
	bash tests/runtests.sh

clearscr:
	clear

clean:
	rm -rf _build main

%.native:
	ocamlbuild -use-ocamlfind $@
	mv $@ $*

.PHONY: default test runtests clearscr clean
