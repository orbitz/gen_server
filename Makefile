.PHONY: all clean test examples

all:
	$(MAKE) -C lib

test:
	$(MAKE) -C lib test
	$(MAKE) -C tests test

examples: all
	$(MAKE) -C examples

clean:
	$(MAKE) -C lib clean
	$(MAKE) -C tests clean
	$(MAKE) -C examples clean

