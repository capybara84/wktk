SRC = env.ml syntax.ml symbol.ml lexer.ml parser.ml eval.ml type.ml builtins.ml test.ml wktk.ml

all : wktk

wktk : $(SRC)
	ocamlopt -o $@ $^

test : wktk
	./wktk -t

clean:
	rm -f *.cmi *.cmx *.cmo *.o wktk

