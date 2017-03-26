signature FRAME =
sig type frame
    type access
    val newFrame: {name: Temp.label, kFormals: bool list, moreFormals:access list} -> frame
    val name: frame -> Temp.label
    val formals: frame -> access list
    val allocLocal: frame -> bool -> access
    val FP : Temp.temp
    datatype frag =  PROC of {body:Tree.stm, frame:frame}
                   | STRING of Temp.label * string
    val wordSize : int
    val RV : Temp.temp
    val procEntryExit1 : frame * Tree.stm -> Tree.stm
    val exp : access -> Tree.exp -> Tree.exp (* first Tree.exp is address of the stack frame that the access lives in *)
    val string: (Temp.label * string) -> string
end
