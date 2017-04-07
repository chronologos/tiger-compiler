signature CODEGEN =
sig
    val codegen : MipsFrame.frame -> Tree.stm -> Assem.instr list
end
