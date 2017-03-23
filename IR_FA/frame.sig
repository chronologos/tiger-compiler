signature FRAME =
sig type frame
    type access
    val newFrame: {name: Temp.label, kFormals: bool list, moreFormals:access list} -> frame
    val name: frame -> Temp.label
    val formals: frame -> access list
    val allocLocal: frame -> bool -> access
    val FP : Temp.temp
    val wordSize : int
    val exp : access -> Tree.exp -> Tree.exp (* first Tree.exp is adress of the stack frame that the access lives in *)
end
