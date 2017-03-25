(* make this an abstraction sometime *)
structure Temp : TEMP =
struct
    type temp = string (* TODO *)
    val temps = ref 100
    fun newtemp() = let val t = !temps in temps := t+1; Int.toString(t) end

    fun newNamedTemp(title:string) = let val t = (Int.toString(!temps)^":"^title) 
                                     in temps := !temps+1; t end

    (*structure Table = IntMapTable(type key = string
				  fun getInt n = n)*)
				  
    fun makestring t = "t" ^ t
    (*fun makestring t = "t" ^ Int.toString t*)

  type label = Symbol.symbol

local structure F = Format
      fun postinc x = let val i = !x in x := i+1; i end
      val labs = ref 0
 in
    fun newlabel() = Symbol.symbol(F.format "L%d" [F.INT(postinc labs)])
    fun namedlabel(title:string) = Symbol.symbol(F.format("L%d") [F.INT(postinc labs)] ^ ":" ^ title)
end


end
