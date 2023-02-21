OCAMLC = ocamlc
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex

all: o

o: ast.cmo parser.cmo scanner.cmo p.cmo
	$(OCAMLC) -o o ast.cmo parser.cmo scanner.cmo p.cmo

ast.cmo: ast.ml
	$(OCAMLC) -c ast.ml

parser.cmi: parser.mli ast.cmo
	$(OCAMLC) -c parser.mli

parser.cmo: parser.ml parser.cmi
	$(OCAMLC) -c parser.ml

scanner.cmo: scanner.ml parser.cmi
	$(OCAMLC) -c scanner.ml

scanner.ml: scanner.mll
	$(OCAMLLEX) scanner.mll

p.cmo: p.ml ast.cmo parser.cmo scanner.cmo
	$(OCAMLC) -c p.ml

clean:
	rm -f *.cmo *.cmi parser.ml parser.mli scanner.ml o

.PHONY: all clean
