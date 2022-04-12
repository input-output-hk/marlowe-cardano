#!/usr/bin/env nix-shell
#!nix-shell -i "make -f" -p gnumake


SUBDIRS:=simple zcb escrow cfd swap coveredCall


all: $(SUBDIRS)

$(SUBDIRS):
	make -C $@ docs

clean:
	-rm */tx-?.* */utxo-?-?.json */*.{diff,log}

.SUFFIXES:

.PHONY: $(SUBDIRS)
