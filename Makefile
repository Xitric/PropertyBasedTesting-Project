all:
	make expressionGenerator
	make pipelineGenerator
	make scaffoldingGenerator
	make utils

expressionGenerator:
	ocamlbuild -use-ocamlfind -package qcheck expressionGenerator.byte
	ocamlbuild -use-ocamlfind -package qcheck expressionGenerator.cma

pipelineGenerator:
	ocamlbuild -use-ocamlfind -package qcheck pipelineGenerator.byte
	ocamlbuild -use-ocamlfind -package qcheck pipelineGenerator.cma

scaffoldingGenerator:
	ocamlbuild -use-ocamlfind -package qcheck scaffoldingGenerator.byte
	ocamlbuild -use-ocamlfind -package qcheck scaffoldingGenerator.cma

utils:
	ocamlbuild -use-ocamlfind -lib unix utils.byte
	ocamlbuild -use-ocamlfind -lib unix utils.cma

.PHONY: testMain
testMain:
	make all
	ocamlbuild -use-ocamlfind -package qcheck main.byte
	ocamlbuild -use-ocamlfind -package qcheck main.cma
	./main.byte

clean:
	ocamlbuild -clean