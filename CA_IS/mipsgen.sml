structure MipsGen :> CODEGEN = struct
  structure A = Assem and T = Tree and S = Symbol
  structure Frame = MipsFrame
  fun codegen frame stm : A.instr list = [A.NOTHING]
end
