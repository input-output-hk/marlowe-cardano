#!/usr/bin/env nix-shell
#!nix-shell -i "make -f" -p gnumake


DOCS=$(wildcard *-*.md)

TESTS=$(addsuffix .log,$(basename $(wildcard run-*.sh)))


all: tests docs

tests: $(TESTS)

run-%.log: run-%.sh
	bash -o verbose $< |& tee $@

docs: $(DOCS)

%.md : run-%.sh
	../log2md.sh $< $@


.SUFFIXES:

.PHONY: all tests docs
