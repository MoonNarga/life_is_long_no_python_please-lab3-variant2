# CPO_life_is_long_no_python_please

## Description

### Objectives

- Design an embedded domain-specific language or a simple text parser.

- Design architecture of a simple interpreter.

- Develop a simple interpreter for a specific model of computation.

- Develop unit tests.

- Develop input data control in the aspect-oriented style.
  
In the second laboratory work, students should develop a simple interpreter
for the specific model of computation. For that, you need:

1. Design an input language. Depends on your variant, it should be:

   - an embedded domain-specific (eDSL) language for building computational
process description

   - a string with specific syntax and semantic.

2. Design types for MoC object representation and interpreter, which allows
your library user to execute a computational process description.

3. Implement an interpreter for computational process models (descriptions)
. User should be possible to:

   - define different computational process models

   - execute them separately with computational process trace

   - execute them with different input data

   - visualize computational process models.

4. Testing. Should include:

   - several simple examples of interpreter usages

   - at least one complex example

   - cornet cases.

5. Input data validation in aspect-oriented style.

## Variant

- No.2 Mathematical expression by string substitution

## Struct of project

- The calculator.hs in app/ is the implementation.

- The test runs by the cabal, and the test cases are in test/*.hs.

## Key features

- The use of Haskell.

- Achieve a calculator works like `REPL`

## Design note

In the begining, I tried to load the expression byte by byte
and eval it by stack, but it's really inefficient.

Finally I imitate others to implement it with Parsec, it's
so convient to convert every part of the string into Expressions.

## Contribution

Zhao, Tianhao is responsible for the implementation of the
data structure and some test cases.

Ge, Binghang edit the Readme and contribute some test cases,
also the action.