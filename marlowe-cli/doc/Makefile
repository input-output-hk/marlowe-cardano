#!/usr/bin/env nix-shell
#!nix-shell -i "make -f" -p gnumake pandoc


DATE=$(shell date +"%d %B %Y")
VERSION=$(shell marlowe-cli --version)

DOCS=$(wildcard *.ipynb)

MDS=$(addsuffix .md,$(basename $(DOCS)))

PNGS=$(addsuffix .png,$(basename $(wildcard diagrams/*.dot)))
SVGS=$(addsuffix .svg,$(basename $(wildcard diagrams/*.dot)))


all: ../ReadMe.md $(MDS) $(PNGS) $(SVGS)


%.md: %.ipynb
	pandoc -f ipynb -t gfm \
	       --metadata jupyter= \
	       --metadata date="$(DATE)" \
	       --metadata version="$(VERSION)" \
	       -s $< -o $@
	sed -i -e '/^:::/d; s/ {#example}$$//' $@

%.png: %.dot
	dot -Tpng -o $@ $<

%.svg: %.dot
	dot -Tsvg -o $@ $<


.PRECIOUS: %.md %.png %.svg

.SUFFIXES:

.PHONY: all
