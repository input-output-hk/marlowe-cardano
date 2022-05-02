#!/usr/bin/env nix-shell
#!nix-shell -i "make -f" -p gnumake


TESTS=$(addsuffix .log,$(basename $(wildcard run-*.sh)))


tests: $(TESTS)

run-%.log: run-%.sh
	bash -o verbose $< |& tee $@

docs: ReadMe.md

ReadMe.md : run-swap.sh
	../log2md.sh $< $@

clean:
	-rm tx-?.{marlowe,contract,state,raw} *.{diff,log} utxo-?-?.json


.SUFFIXES:

.PHONY: all tests docs clean
