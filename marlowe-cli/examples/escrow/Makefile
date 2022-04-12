#!/usr/bin/env nix-shell
#!nix-shell -i "make -f" -p gnumake


DOCS=$(wildcard *-*.md)

TESTS=$(addsuffix .log,$(basename $(wildcard run-*.sh)))


tests: $(TESTS)

run-%.log: run-%.sh
	bash -o verbose $< |& tee $@

docs: $(DOCS)

%.md : run-%.sh
	../log2md.sh $< $@

clean:
	-rm tx-?.{marlowe,contract,state,raw} *.{diff,log}

.SUFFIXES:

.PHONY: all tests docs clean
