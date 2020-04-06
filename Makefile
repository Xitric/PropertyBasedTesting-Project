all:
	make expressionGenerator
	make pipelineGenerator

expressionGenerator:
	ocamlbuild -use-ocamlfind -package qcheck expressionGenerator.byte
	ocamlbuild -use-ocamlfind -package qcheck expressionGenerator.cma

pipelineGenerator:
	ocamlbuild -use-ocamlfind -package qcheck pipelineGenerator.byte
	ocamlbuild -use-ocamlfind -package qcheck pipelineGenerator.cma

clean:
	ocamlbuild -clean