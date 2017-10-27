# tiger-compiler
Tiger compiler implemented in SML, with backend agnostic IR.

# Project structure:
Each of the following folders represents the compiler at a certain stage. Code within each folder is self-contained.

## Compiler Frontend
1. Lexer 	      -> Transforming tiger into tokens.
2. Parser 	    -> Transforming tokens into an abstract syntax tree (AST)
3. TypeChecker  -> Code that typechecks AST. 
4. IR_FA 	      -> Transforming AST into Intermediate Representation + code that does Frame Analysis
## Compiler Backend
5. CA_IS 	    -> Canonicalization and instruction selection code
6. DA_LI 	    -> Dataflow and Liveness Analysis and Register Allocation code
7. FINAL 	    -> Complete Compiler
