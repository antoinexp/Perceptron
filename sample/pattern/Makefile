OCAMLBUILD = ocamlbuild
BIN = pattern
LIBDIR=./../../../_build
FLAGS = -I,$(LIBDIR)
LIBS=graphics,perceptron


all: byte

native:
	@$(OCAMLBUILD) -lflags $(FLAGS) -cflags $(FLAGS) -libs $(LIBS) $(BIN).native

byte:
	@$(OCAMLBUILD) -lflags $(FLAGS) -cflags $(FLAGS) -libs $(LIBS) $(BIN).byte

clean:
	$(OCAMLBUILD) -clean
