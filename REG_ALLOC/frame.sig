signature FRAME =
sig type frame
    type access
    val newFrame: {name: Temp.label, kFormals: bool list, moreFormals:access list} -> frame
    val name: frame -> Temp.label
    val formals: frame -> access list
    val allocLocal: frame -> bool -> access
    
    val FP : Temp.temp
    val RA : Temp.temp
    val RV : Temp.temp
    val SP : Temp.temp
    val ZERO : Temp.temp
    
    datatype frag =  PROC of {body:Tree.stm, frame:frame}
                   | STRING of Temp.label * string
                   
    val wordSize : int
    val k:int
    val procEntryExit1 : (frame * Tree.stm) -> Tree.stm
    val procEntryExit2 : (frame * Assem.instr list) -> Assem.instr list
    val procEntryExit3 : (frame * Tree.stm) -> {prolog:string, body:Tree.stm, epilog:string}
    val exp : access -> Tree.exp -> Tree.exp (* first Tree.exp is address of the stack frame that the access lives in *)
    val string: (Temp.label * string) -> string
    
    val argregs: Temp.temp list 
    val specialregs: Temp.temp list
    val calleesaves: Temp.temp list
    val callersaves: Temp.temp list
    
end
