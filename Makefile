all: main

main: ast.cmo  parser.cmi  lexer.cmo interpreter.cmo parser.cmo  main.cmo
	ocamlc -o main ast.cmo  parser.cmo lexer.cmo interpreter.cmo  main.cmo

lexer.ml: lexer.mll parser.cmi
	ocamllex lexer.mll

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

ast.cmo: ast.ml
	ocamlc -c ast.ml

interpreter.cmo: interpreter.ml ast.cmo
	ocamlc -c interpreter.ml

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi ast.cmo
	ocamlc -c parser.ml

lexer.cmo: lexer.ml parser.cmi ast.cmo
	ocamlc -c lexer.ml

main.cmo: main.ml ast.cmo interpreter.cmo parser.cmi parser.cmo lexer.cmo
	ocamlc -c main.ml

clean:
	rm -f *.cmi *.cmo *.cmx *.o main lexer.ml parser.ml parser.mli

.PHONY: all clean