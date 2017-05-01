signature REG_ALLOC =
sig
  structure Frame : FRAME
  type allocation = Temp.temp Temp.Map.map
  val alloc : Assem.instr list * Frame.frame
              -> Assem.instr list * allocation
  val makeSayTemp: allocation -> (Temp.temp -> string) 
end