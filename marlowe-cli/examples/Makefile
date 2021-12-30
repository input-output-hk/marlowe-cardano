#!/usr/bin/env nix-shell
#!nix-shell -i "make -f" -p gnumake


SUBDIRS:=simple swap escrow


all: $(SUBDIRS)

$(SUBDIRS): 
	make -C $@ tests


.SUFFIXES:

.PHONY: $(SUBDIRS)
