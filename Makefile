OCAMLBUILD = ocamlbuild
BIN = perceptron

all: byte native

native:
	$(OCAMLBUILD) $(BIN).cmxa

byte:
	$(OCAMLBUILD) $(BIN).cma

mli:
	$(OCAMLBUILD) $(BIN).inferred.mli
	

doc:
	$(OCAMLBUILD) $(BIN).docdir/index.html

clean:
	$(OCAMLBUILD) -clean

