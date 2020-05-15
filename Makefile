all:
	make expressionGenerator
	make pipelineGenerator
	make scaffoldingGenerator
	make utils

expressionGenerator:
	ocamlbuild -use-ocamlfind expressionGenerator.byte
	ocamlbuild -use-ocamlfind expressionGenerator.cma

pipelineGenerator:
	ocamlbuild -use-ocamlfind pipelineGenerator.byte
	ocamlbuild -use-ocamlfind pipelineGenerator.cma

scaffoldingGenerator:
	ocamlbuild -use-ocamlfind scaffoldingGenerator.byte
	ocamlbuild -use-ocamlfind scaffoldingGenerator.cma

utils:
	ocamlbuild -use-ocamlfind utils.byte
	ocamlbuild -use-ocamlfind utils.cma

traverseSyntaxTree:
	ocamlbuild -use-ocamlfind traverseSyntaxTree.byte
	ocamlbuild -use-ocamlfind traverseSyntaxTree.cma

.PHONY: stat
stat:
	make all
	ocamlbuild -use-ocamlfind expressionStatistics.byte
	ocamlbuild -use-ocamlfind expressionStatistics.cma
	./expressionStatistics.byte

.PHONY: tests
expressionTests:
	make all
	ocamlbuild -use-ocamlfind expressionTests.byte
	ocamlbuild -use-ocamlfind expressionTests.cma
	./expressionTests.byte

.PHONY: testMain
testMain:
	make all
	ocamlbuild -use-ocamlfind main.byte
	ocamlbuild -use-ocamlfind main.cma
	./main.byte

.PHONY: executionTests
executionTests:
	make all
	ocamlbuild -use-ocamlfind executionTests.byte
	ocamlbuild -use-ocamlfind executionTests.cma
	./executionTests.byte

clean:
	ocamlbuild -clean