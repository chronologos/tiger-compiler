(* make this an abstraction sometime *)
structure Temp : TEMP =
struct
  type temp = string (* TODO *)
  val temps = ref 100
  val labelCount = ref 0

  fun newtemp() =
    let
      val t = !temps
    in
      temps := t+1;
      Int.toString(t)
    end

  fun newNamedTemp(title:string) =
    let
      val t = (Int.toString(!temps)^":"^title)
    in
      temps := !temps+1;
      t
   end

  fun makestring t = "t" ^ t
  val compare = String.compare

  type label = Symbol.symbol
  structure TempOrd =
  struct
    type ord_key = temp
    val compare = compare
  end

  structure Set = SplaySetFn(TempOrd)
  structure Map = SplayMapFn(TempOrd)

  local structure F = Format
      fun postinc x = let val i = !x in x := i+1; i end
      val labs = ref 0
  in
    fun newlabel() = Symbol.symbol(F.format "L%d" [F.INT(postinc labs)])
    fun namedlabel(title:string) = Symbol.symbol(F.format("L%d") [F.INT(postinc labs)] ^ ":" ^ title)
  end
end
