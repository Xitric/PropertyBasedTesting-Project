all:
	make expressionGenerator
	make pipelineGenerator
	make scaffoldingGenerator

expressionGenerator:
	ocamlbuild -use-ocamlfind -package qcheck expressionGenerator.byte
	ocamlbuild -use-ocamlfind -package qcheck expressionGenerator.cma

pipelineGenerator:
	ocamlbuild -use-ocamlfind -package qcheck pipelineGenerator.byte
	ocamlbuild -use-ocamlfind -package qcheck pipelineGenerator.cma

scaffoldingGenerator:
	ocamlbuild -use-ocamlfind -package qcheck scaffoldingGenerator.byte
	ocamlbuild -use-ocamlfind -package qcheck scaffoldingGenerator.cma

clean:
	ocamlbuild -clean