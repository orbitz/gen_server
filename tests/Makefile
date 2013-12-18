CAMLP4=
OCAMLC_OPTS=-package core,async,gen_server
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

TEST_MODULES=success.ml init_exn.ml handle_call_exn.ml terminate_exn.ml

TEST_DEPS=../lib/gen_server/gen_server.cmxa ../lib/gen_server/gen_server.cma

.PHONY: all test

all: test

# Dependencies
include ../lib/Ocamlrules.mk.in
