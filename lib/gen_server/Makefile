CAMLP4=

OCAMLDEP_OPTS=-package core,async
OCAMLC_OPTS=$(OCAMLDEP_OPTS) -w A
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

LIB_MODULES=gen_server.ml
NON_LIB_MODULES=

BYTE_TARGETS=gen_server.cma
NATIVE_TARGETS=gen_server.cmxa

.PHONY: all test

all: native-code byte-code

install: all
	ocamlfind install gen_server META gen_server.cmxa gen_server.cma gen_server.a *.cmx *.cmi *.mli

test:

# Dependencies
include ../Ocamlrules.mk.in
