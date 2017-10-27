# tiger-compiler
Tiger compiler implemented in SML, with backend agnostic IR.

# Project structure:
Each of the following folders represents the compiler at a certain stage. Code within each folder is self-contained.

## Compiler Frontend
Lexer 	    -> Transforming tiger into tokens.
Parser 	    -> Transforming tokens into an abstract syntax tree (AST)
TypeChecker -> Code that typechecks AST. 
IR_FA 	    -> Transforming AST into Intermediate Representation + code that does Frame Analysis
## Compiler Backend
CA_IS 	    -> Canonicalization and instruction selection code
DA_LI 	    -> Dataflow and Liveness Analysis and Register Allocation code
FINAL 	    -> Complete Compiler
