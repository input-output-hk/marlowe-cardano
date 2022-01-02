#!/usr/bin/env nix-shell
#!nix-shell -i "make -f" -p gnumake


SUBDIRS:=simple swap zcb escrow


all: $(SUBDIRS)

$(SUBDIRS):
	make -C $@ clean tests


.SUFFIXES:

.PHONY: $(SUBDIRS)
