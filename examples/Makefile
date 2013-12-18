CAMLP4=
OCAMLC_OPTS=-package gen_server
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

NON_LIB_MODULES=simple.ml vending_machine.ml vending_machine_functor.ml

native_bin=$(NON_LIB_MODULES:%.ml=%.native)

byte_bin=$(NON_LIB_MODULES:%.ml=%.byte)

.PHONY: all native-code byte-code test clean

all: native-code byte-code

native-code: $(native_bin)

byte-code: $(byte_bin)

clean: extra_clean

extra_clean:
	-rm -f *.native *.byte

# Dependencies
include ../lib/Ocamlrules.mk.in
