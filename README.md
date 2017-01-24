Lexer
=====
Main code in `tiger.lex`. `driver.sml` is test framework, `sources.cm` is makefile and `tokens.sig` defines tokens to lex into.

To run the lexer, open sml and type the following commands:

CM.make "sources.cm";
Parse.parse "tests/test1.tig";
