structure Translate :> TRANSLATE =
struct
  type level = int
  type access = level * Frame.access

  val frameTable = IntBinaryMap.empty (* level -> Frame.frame map *)
  val outermost = 0

  (* call Frame.newFrame to create new frame, add (level,frame) to table *)
  fun newLevel ({parent=lev, name=label, formals=formals}) =
    let
        val nextLevel = lev+1
        val nextFrame = Frame.newFrame({name=label, formals=formals})
        val frameTable = IntBinaryMap.insert(frameTable, nextLevel, nextFrame)
    in
        nextLevel
    end

  fun formals level =
    let
        val frameAccess = getFrameAccessList level
        fun foldFn (acc,list) = (level,acc)::list
    in
        foldr foldFn [] frameAccess
    end

  fun getFrameAccessList level =
    let
      val frame = IntBinaryMap.find(frameTable,level)
    in
      case frame of
        NONE => (print("Unable to access formals at level "^level^". Level does not exist.\n"); [])
      | SOME(f) => Frame.formals(f)
    end


  fun allocLabel level =
    let
        frame = IntBinaryMap.find(frameTable,level)
        fun labelAccess ecp =
          case frame of
            SOME(f) => (level, Frame.allocLocal f ecp)
          | NONE => (
            print("Frame at level "^level^" does not exist.\n"); (-1,Frame.newFrame({name=Temp.newLabel(),formals=[]}))
            )
    in
      labelAccess
    end

end
