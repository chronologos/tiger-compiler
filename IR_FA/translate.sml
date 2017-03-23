(*
  level should monotonically increase, store pointer to parent level (new hash table),
  frameTable map not actually updated ; replace intBinaryMap with HashTable
 *)
structure Translate :> TRANSLATE =
struct
  structure H = HashTable
  structure Frame = MipsFrame
  structure T = Tree
  type level = int *  unit ref
  type access = level * Frame.access
  datatype exp = Ex of Tree.exp
                | Nx of Tree.stm
                | Cx of Temp.label * Temp.label -> Tree.stm


  exception Translate

  val debug = false
  val k = 4

  (* val currentLevel = ref 0 *)
  val outermost = (0,ref ())
  val outermostFrame = SOME(Frame.newFrame({name=Temp.newlabel(), kFormals=[], moreFormals=[]}))
  val sizeHintFrameTable = 16
  val sizeHintLevelTable = 128
  val frameTable : (level,Frame.frame) H.hash_table =
  		H.mkTable(fn (x,xref) => Word.fromInt(x), op = ) (sizeHintFrameTable,Translate)
(*  val levelTable : (level, level) H.hash_table =
      H.mkTable(fn x:int => Word.fromInt(x), op =) (sizeHintLevelTable,Translate)
*)

(*  fun symbol name =
      case H.find hashtable name
       of SOME i => (name,i)
        | NONE => let val i = !nextsym
	           in nextsym := i+1;
		      H.insert hashtable (name,i);
		      (name,i)
		  end
*)
  (* a=access of var, l=level of function in which var is used *)
  fun simpleVar(a:access,l:level) =
    let
        val (dLevInt,fAccess) =  case a of
                                  ((levInt, levRef),frameAccess) => (levInt,frameAccess)
        val useLevInt = case l of
                          (levInt,levRef) => levInt
        val fpExp = T.TEMP Frame.FP
    in
        if useLevInt = dLevInt
        then Frame.exp(fAccess)(fpExp)
        else (
            let val levDiff = useLevInt - dLevInt
                fun getfp(diff, tExp) =
                    if diff=0
                    then tExp
                    else getfp(diff-1, T.MEM (tExp))
            in
                Frame.exp(fAccess)(getfp(levDiff,fpExp))
            end
        )
    end

  fun seq(stmList) =
    let foldStmFn (stm, seqStm) = T.SEQ (stm,seqStm)
    in  foldl foldStmFn List.hd(stmList) List.drop(stmList,1)
    end

  fun unEx (Ex e) = e
    | unEx (Cx genstm) =
        let val r = Temp.newtemp()
            val t = Temp.newlabel() and f = Temp.newlabel()
        in  T.ESEQ(seq[T.MOVE(T.TEMP r, T.CONST 1),
                       genstm(t,f),
                       T.LABEL f,
                       T.MOVE(T.TEMP r,T.CONST 0),
                       T.LABEL t],
                    T.TEMP r)
        end
    | unEx(Nx s) = T.ESEQ(s,T.CONST 0)

  fun unNx (Ex e) = T.EXP e
    | unNx (Cx genstm) =
        let l1 = Temp.newlabel()
        in  seq([genstm(l1,l1),
                 T.LABEL l1])
        end
    | unNx (Nx s) = s

  fun unCx (Cx genstm) = genstm
    | unCx (Ex e) =
        case e of
          T.CONST 1 =>
            fn (l1,l2) => T.JUMP(T.NAME l1, [l1])
        | T.CONST 0 =>
            fn (l1,l2) => T.JUMP(T.NAME l2, [l2])
        | (_) =>
        let val t = Temp.newlabel()
            val f = Temp.newlabel()
            val z = T.CONST 0
            fun cxFn (l1,l2) =
              seq([T.CJUMP(T.EQ,z,e,t,f),
                   T.LABEL t,
                   T.JUMP (T.NAME l1, [l1])
                   T.LABEL f,
                   T.JUMP (T.NAME l2, [l2])
                  ])
        in
            cxFn
        end
    | unCx (Nx s) =
        let val t = Temp.newlabel()
            (*  should be syntax error to unCx an Nx *)
            fun cxFn (l1,l2) =
                seq([T.LABEL l2])
        in
          cxFn
        end


  fun debugPrint(msg:string, pos:int) =
    if debug
    then ErrorMsg.error pos msg
    else ()

  fun levelToString (lev:level) =
    case lev of
      (lint, lref) => Int.toString(lint)
  (* call Frame.newFrame to create new frame, add (level,frame) to frame table, add (level, parent) to levelTable *)
  fun newLevel ({parent=lev:level, name=label, formals=formals}) =
    let
        val nextLevel = case lev of (lint, lref) => (lint+1, ref ())
        val parentFrameOpt = H.find frameTable lev
        val nextFrame = if List.length(formals) <= k
                        then Frame.newFrame({name=label, kFormals=formals, moreFormals=[]})
                        else  case parentFrameOpt of
                                SOME(parentFrame) => (
                                  let
                                    val moreFormals = List.drop(formals,k)
                                    fun foldFormalsFn (bool,accessList) =
                                        (Frame.allocLocal parentFrame true) :: accessList
                                    val accessMoreFormals = List.drop(foldr foldFormalsFn [] formals,k)
                                  in
                                    Frame.newFrame({name=label, kFormals=List.take(formals,k), moreFormals=accessMoreFormals})
                                  end
                                )
                              | NONE => (ErrorMsg.error 0 ("[ TRANSLATE ] Parent frame at level "^levelToString(lev)^" not found.\n"); Frame.newFrame({name=label,kFormals=[],moreFormals=[]}))
    in
        H.insert frameTable (nextLevel,nextFrame);
        if debug
        then print("Translate.newLevel "^levelToString(nextLevel)^" created for label "^Symbol.name(label)^".\n")
        else ();
        nextLevel
    end

  fun getFrameAccessList level =
    let
      val frame = if level=outermost then outermostFrame else  H.find frameTable level
    in
      case frame of
        NONE => (ErrorMsg.error 0 ("Unable to access formals at level "^levelToString(level)^". Level does not exist.\n"); [])
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
              debugPrint("Translate.allocLocal called with escape "^Bool.toString(ecp)^" at level "^levelToString(level)^".\n",0);
              (level, Frame.allocLocal f ecp)
            )
          | NONE => (
            ErrorMsg.error 0 ("Frame at level "^levelToString(level)^" does not exist.\n");
            ((0-1, ref ()),Frame.allocLocal(Frame.newFrame({name=Temp.newlabel(), kFormals=[], moreFormals=[]}))(ecp) )
            )

    in
      debugPrint("Translate.allocLocal at level "^levelToString(level)^" called"^".\n",0);
      labelAccess
    end

end
