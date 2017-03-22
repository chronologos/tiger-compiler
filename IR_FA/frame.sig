signature FRAME =
sig type frame
    type access
    val newFrame: {name: Temp.label, kFormals: bool list, moreFormals:access list} -> frame
    val name: frame -> Temp.label
    val formals: frame -> access list
    val allocLocal: frame -> bool -> access
end
