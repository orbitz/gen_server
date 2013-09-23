CAMLP4=
OCAMLC_OPTS=-package gen_server
OCAMLOPT_OPTS=$(OCAMLC_OPTS)

NON_LIB_MODULES=simple.ml vending_machine.ml vending_machine_functor.ml

native_bin=$(NON_LIB_MODULES:%.ml=%.native)

byte_bin=$(NON_LIB_MODULES:%.ml=%.byte)

.PHONY: all native-code byte-code test clean

all: native-code byte-code

native-code: $(native_bin)

simple.native: simple.cmx

vending_machine.native: vending_machine.cmx

vending_machine_functor.native: vending_machine_functor.cmx

byte-code: $(byte_bin)

simple.byte: simple.cmo

vending_machine.byte: vending_machine.cmo

vending_machine_functor.byte: vending_machine_functor.cmo

clean: extra_clean

extra_clean:
	-rm -f *.native *.byte *.cmx *.cmo *.o *.cmi

# Dependencies
include ../lib/Ocamlrules.mk.in
