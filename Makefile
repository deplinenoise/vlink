# Standard Unix

DESTDIR ?= /usr/local

TARGET = vlink
DIR = objects

CC = gcc
CCOUT = -o $(DUMMYVARIABLE)	# produces the string "-o "
COPTS = -O2 -fomit-frame-pointer -c
CONFIG =

LD = gcc
LDOUT = -o $(DUMMYVARIABLE)	# produces the string "-o "
LDOPTS =
LIBS =
INSTALL = install

include make.rules

install: vlink vlink.pdf
	$(INSTALL) -D vlink $(DESTDIR)/bin/vlink
	$(INSTALL) -D vlink.pdf $(DESTDIR)/share/doc/vlink/vlink.pdf
