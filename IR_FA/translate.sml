(*
  level should monotonically increase, store pointer to parent level (new hash table),
  frameTable map not actually updated ; replace intBinaryMap with HashTable
 *)
structure Translate :> TRANSLATE =
struct
  type level = int
  type access = level * Frame.access
  structure H = HashTable
  exception Translate

  val currentLevel = ref 0
  val outermost = 0
  val sizeHintFrameTable = 16
  val sizeHintLevelTable = 128
  val frameTable : (int,Frame.frame) H.hash_table =
  		H.mkTable(fn x => x, op = ) (sizeHintFrameTable,Translate)
  val levelTable : (level, level) H.hash_table =
      H.mkTable(fn x=> x, op =) (sizeHintLevelTable,Translate)

(*  fun symbol name =
      case H.find hashtable name
       of SOME i => (name,i)
        | NONE => let val i = !nextsym
	           in nextsym := i+1;
		      H.insert hashtable (name,i);
		      (name,i)
		  end
*)


  (* call Frame.newFrame to create new frame, add (level,frame) to frame table, add (level, parent) to levelTable *)
  fun newLevel ({parent=lev, name=label, formals=formals}) =
    let
        val nextLevel = !currentLevel+1
        currentLevel := nextLevel
        val nextFrame = Frame.newFrame({name=label, formals=formals})
    in
        H.insert frameTable (nextLevel,nextFrame);
        H.insert levelTable (nextLevel,lev);
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
      val frame = H.find frameTable level
    in
      case frame of
        NONE => (ErrorMsg.error 0 "Unable to access formals at level "^level^". Level does not exist.\n"; [])
      | SOME(f) => Frame.formals(f)
    end


  fun allocLocal level =
    let
        val frame = H.find frameTable level
        fun labelAccess ecp =
          case frame of
            SOME(f) => (level, Frame.allocLocal f ecp)
          | NONE => (
            ErrorMsg.error 0 "Frame at level "^level^" does not exist.\n"; (-1,Frame.newFrame({name=Temp.newLabel(),formals=[]}))
            )
    in
      labelAccess
    end

end
