.PHONY: 	all clean byte native profile debug sanity test

OCB_FLAGS   = -cflag -w -cflag -40 -use-ocamlfind -pkg unix
OCB = ocamlbuild $(OCB_FLAGS)

all: native byte # profile debug

clean:
	$(OCB) -clean

native: sanity
	$(OCB) src/Main.native

byte: sanity
	$(OCB) src/Main.byte

profile: sanity
	$(OCB) -tag profile src/Main.native

debug: sanity
	$(OCB) -tag debug src/Main.byte

# check that menhir is installed, use "opam install menhir"
sanity:

test: native
	./src/main.native "2 + 3 * 3"
