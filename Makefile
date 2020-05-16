all:
	make expressionGenerator
	make pipelineGenerator
	make scaffoldingGenerator
	make pipelineModel
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

pipelineModel:
	ocamlbuild -use-ocamlfind pipelineModel.byte
	ocamlbuild -use-ocamlfind pipelineModel.cma

utils:
	ocamlbuild -use-ocamlfind utils.byte
	ocamlbuild -use-ocamlfind utils.cma

.PHONY: stat
stat:
	make all
	ocamlbuild -use-ocamlfind expressionStatistics.byte
	ocamlbuild -use-ocamlfind expressionStatistics.cma
	./expressionStatistics.byte

.PHONY: expressionTests
expressionTests:
	make all
	ocamlbuild -use-ocamlfind expressionTests.byte
	ocamlbuild -use-ocamlfind expressionTests.cma
	./expressionTests.byte

.PHONY: dslTests
dslTests:
	make all
	ocamlbuild -use-ocamlfind dslTests.byte
	ocamlbuild -use-ocamlfind dslTests.cma
	./dslTests.byte

.PHONY: executionTests
executionTests:
	make all
	ocamlbuild -use-ocamlfind executionTests.byte
	ocamlbuild -use-ocamlfind executionTests.cma
	./executionTests.byte

clean:
	ocamlbuild -clean