.PHONY:     all clean byte native profile debug sanity

OCB_FLAGS   = -cflag -w -cflag -40 -use-ocamlfind -use-menhir
OCB = ocamlbuild $(OCB_FLAGS)

all: native byte profile debug

clean:
	$(OCB) -clean

native: sanity
	$(OCB) main.native

byte: sanity
	$(OCB) main.byte

profile: sanity
	$(OCB) -tag profile main.native

debug: sanity
	$(OCB) -tag debug main.byte

# check that menhir is installed, use "opam install menhir"
sanity:
	which menhir

run: native
	./main.native
