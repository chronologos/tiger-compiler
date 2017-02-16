# Shift-Reduce Conflicts
We have 2 shift-reduce conflicts corresponding to the following states. 
  `dec : tydecs .  (reduce by rule 55)`
  `tydecs : tydecs . tydec`

  `dec : fundecs .  (reduce by rule 57)`
  `fundecs : fundecs . fundec`
This happens because we can either shift to add another tydec/fundec to the tydecs/fundecs, or we can reduce the current tydecs/fundecs to a dec. The correct behaviour is to shift since we want as many contiguous fundecs/tydecs in a list as possible. ML-YACC's default behaviour is to shift, therefore these two SR conflicts are safe.

# Command to print abstract syntax tree
`PrintAbsyn.print(TextIO.stdOut, Parse.parse("../tests/fundectest.tig"));`

