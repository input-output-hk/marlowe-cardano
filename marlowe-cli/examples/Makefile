#!/usr/bin/env nix-shell
#!nix-shell -i "make -f" -p gnumake


SUBDIRS:=simple swap zcb escrow


all: $(SUBDIRS)

$(SUBDIRS):
	make -C $@ clean tests

clean:
	-rm */tx-?.* */utxo-?-?.json */*.{diff,log}

.SUFFIXES:

.PHONY: $(SUBDIRS)
