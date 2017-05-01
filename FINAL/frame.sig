signature FRAME =
sig type frame
    type access
    val newFrame: (Temp.label * bool list) -> frame
    val name: frame -> Temp.label
    val formals: frame -> access list
    val allocLocal: frame -> bool -> access
    
    val FP : Temp.temp
    val RA : Temp.temp
    val RV : Temp.temp
    val SP : Temp.temp
    val ZERO : Temp.temp
    val ERROR: Temp.temp
    val regsMap: Temp.temp StringMap.map
    val maxCallArgs : frame -> int
    val setCallArgs : frame * int -> unit
    val printAccessList : string * access list -> unit
    
    datatype frag =  PROC of {body:Tree.stm, frame:frame}
                   | STRING of Temp.label * string
                   
    val wordSize : int
    val k:int
    val NUMREG: int
    val getMaxOffset : frame -> int
    val procEntryExit1 : (frame) -> {params:Assem.instr list ,loads:Assem.instr list, saves:Assem.instr list}
    val procEntryExit2 : (frame * Assem.instr list) -> Assem.instr list
    val procEntryExit3 : frame -> {prolog:Assem.instr list, epilog:Assem.instr list}
    val procEntryExit4 : frame -> Assem.instr list
    val exp : access -> Tree.exp -> Tree.exp (* first Tree.exp is address of the stack frame that the access lives in *)
    val string: (Temp.label * string) -> string

    val argregs: Temp.temp list 
    val specialregs: Temp.temp list
    val allRegisters: Temp.Set.set
    val calleesaves: Temp.temp list
    val callersaves: Temp.temp list
    val regTable: Temp.temp Temp.Map.map
    val usableRegisters: Temp.Set.set

end
