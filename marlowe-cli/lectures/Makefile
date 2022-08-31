#!/usr/bin/env nix-shell
#!nix-shell -i "make -f" -p gnumake curl unzip pandoc imagemagick


PNGS=$(addsuffix .png,$(basename $(wildcard diagrams/*.dot)))
SVGS=$(addsuffix .svg,$(basename $(wildcard diagrams/*.dot)))

GIFS=$(wildcard diagrams/escrow-utxos-?.png)


all: $(PNGS) $(SVGS) $(GIFS)


%.png: %.dot
	dot -Tpng -o $@ $<

%.svg: %.dot
	dot -Tsvg -o $@ $<


diagrams/escrow-utxos.gif: $(GIFS)
	convert -delay 500 -loop 0 $^ $@


.PRECIOUS: %.png %.svg %.gif

.SUFFIXES:

.PHONY: all
