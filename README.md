# Testing a Domain Specific Language for IoT Devices
This repository contains a QuickCheck framework written in OCaml for testing a domain specific language (DSL).

## Dependencies
Running this repository requires an installation of OCaml, Java, and Groovy.

## Statistics
Statistics on the code generators can be executed with:

```
make stat
```

## Running the test
The repository contains three tests. The first test verifies that the code generated by the framework is syntactically correct and type correct. It can be run with:

```
make expressionTests
```

The second test verifies that the DSL accepts a vast range of randomly generated, yet legal inputs. It can be run with:

```
make dslTests
```

The third test verifies that the output of the DSL code generator works as expected. Thus, it mus be both syntactically correct and semantically correct. It can be run with:

```
make executionTests
```
