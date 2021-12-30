#!/usr/bin/env nix-shell
#!nix-shell -i "make -f" -p gnumake


TESTS=$(addsuffix .log,$(basename $(wildcard run-*.sh)))


all: tests docs

tests: $(TESTS)

run-%.log: run-%.sh
	bash -o verbose $< |& tee $@

docs: ReadMe.md

ReadMe.md : run-simple.sh
	../log2md.sh $< $@


.SUFFIXES:

.PHONY: all tests docs
