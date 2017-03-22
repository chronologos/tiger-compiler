(*
  level should monotonically increase, store pointer to parent level (new hash table),
  frameTable map not actually updated ; replace intBinaryMap with HashTable
 *)
structure Translate :> TRANSLATE =
struct
  structure H = HashTable
  structure Frame = MipsFrame
  type level = int
  type access = level * Frame.access

  exception Translate

  val debug = false
  val k = 4
  val currentLevel = ref 0
  val outermost = 0
  val outermostFrame = SOME(Frame.newFrame({name=Temp.newlabel(), formals=[]}))
  val sizeHintFrameTable = 16
  val sizeHintLevelTable = 128
  val frameTable : (int,Frame.frame) H.hash_table =
  		H.mkTable(fn x:int => Word.fromInt(x), op = ) (sizeHintFrameTable,Translate)
  val levelTable : (level, level) H.hash_table =
      H.mkTable(fn x:int => Word.fromInt(x), op =) (sizeHintLevelTable,Translate)

(*  fun symbol name =
      case H.find hashtable name
       of SOME i => (name,i)
        | NONE => let val i = !nextsym
	           in nextsym := i+1;
		      H.insert hashtable (name,i);
		      (name,i)
		  end
*)
  fun debugPrint(msg:string, pos:int) =
    if debug
    then ErrorMsg.error pos msg
    else ()

  fun levelToString level =
    Int.toString(level)
  (* call Frame.newFrame to create new frame, add (level,frame) to frame table, add (level, parent) to levelTable *)
  fun newLevel ({parent=lev, name=label, formals=formals}) =
    let
        val nextLevel = !currentLevel+1
        val parentFrameOpt = H.find frameTable lev
        val nextFrame = if List.length(formals) <= k
                        then Frame.newFrame({name=label, kFormals=formals, moreFormals=[]})
                        else  case parentFrameOpt of
                                SOME(parentFrame) => (
                                  let
                                    val moreFormals = List.drop(formals,k)
                                    val foldFormalsFn (bool,accessList) =
                                        (Frame.allocLocal parentFrame true) :: accessList
                                    val accessMoreFormals = List.drop(foldr foldFormalsFn [] kFormals@moreFormals,k)
                                  in
                                    Frame.newFrame({name=label, kFormals=formals, moreFormals=accessMoreFormals})
                                )
                              | NONE => (ErrorMsg.error 0 "[ TRANSLATE ] Parent frame at level "^levelToString(lev)^" not found.\n"; [])
    in
        currentLevel := nextLevel;
        H.insert frameTable (nextLevel,nextFrame);
        H.insert levelTable (nextLevel,lev);
        if debug
        then print("Translate.newLevel "^Int.toString(nextLevel)^" created for label "^Symbol.name(label)^" at parent level "^Int.toString(lev)^".\n")
        else ();
        nextLevel
    end

  fun getFrameAccessList level =
    let
      val frame = if level=outermost then outermostFrame else  H.find frameTable level
    in
      case frame of
        NONE => (ErrorMsg.error 0 ("Unable to access formals at level "^Int.toString(level)^". Level does not exist.\n"); [])
      | SOME(f) => Frame.formals(f)
    end

  fun formals level =
    let
        val frameAccess = getFrameAccessList level
        fun foldFn (acc,list) = (level,acc)::list
    in
        foldr foldFn [] frameAccess
    end

  fun allocLocal level =
    let
        val frame = if level=outermost then outermostFrame else  H.find frameTable level
        fun labelAccess ecp =
          case frame of
            SOME(f) => (
              debugPrint("Translate.allocLocal called with escape "^Bool.toString(ecp)^" at level "^Int.toString(level)^".\n",0);
              (level, Frame.allocLocal f ecp)
            )
          | NONE => (
            ErrorMsg.error 0 ("Frame at level "^Int.toString(level)^" does not exist.\n");
            (0-1,Frame.allocLocal(Frame.newFrame({name=Temp.newlabel(), formals=[]}))(ecp) )
            )

    in
      debugPrint("Translate.allocLocal at level "^Int.toString(level)^" called"^".\n",0);
      labelAccess
    end

end
