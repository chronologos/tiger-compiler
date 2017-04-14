(* make this an abstraction sometime *)
structure Temp : TEMP =
struct
  type temp = int * string * bool (* TODO *)
  val temps = ref 100
  val labelCount = ref 0
  val debug = false

  fun newtemp() =
    let
      val t = !temps
    in
      temps := t+1;
      (t,Int.toString(t),false)
    end

  fun newNamedTemp(title:string) =
    let
      val t = (Int.toString(!temps)^":"^title)
      val prevTemp = !temps
    in
      temps := !temps+1;
      (prevTemp,t,false)
   end

   fun newNamedTempTrue(title:string) =
     let
       val t = (Int.toString(!temps)^":"^title)
     in
       temps := !temps+1;
       (!temps,t,true)
    end

  (*fun getNameFromNamedTemp(namedTemp) =
    (* split by colon and take string after that *)
    let
      fun getStringAfterColon (SOME(x,xs)) = (
        case (Char.toString(x), xs) of (":", _) => implode(xs)
        | (_, []) => ""
        | (_, _) => getStringAfterColon(List.getItem(xs))
      )
      | getStringAfterColon (NONE) = ""
    in
      getStringAfterColon(SOME(chr 1,explode(namedTemp)))
    end *)

  fun makestring (intt,strt,boo) =
      if debug
      then
      "t" ^ strt
      else (if boo
           then "t" ^ strt
           else "t" ^ Int.toString(intt))

  type label = Symbol.symbol


  (*val compare = String.compare

  structure TempOrd =
  struct
    type ord_key = temp
    val compare = compare
  end

  structure Set = SplaySetFn(TempOrd)
  structure Map = SplayMapFn(TempOrd)
*)
  local structure F = Format
      fun postinc x = let val i = !x in x := i+1; i end
      val labs = ref 0
  in
    fun newlabel() = Symbol.symbol(F.format "L%d" [F.INT(postinc labs)])
    fun namedlabel(title:string) = Symbol.symbol(F.format("L%d") [F.INT(postinc labs)] ^ ":" ^ title)
  end
end
