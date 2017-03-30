(*
  level should monotonically increase, store pointer to parent level (new hash table),
  frameTable map not actually updated ; replace intBinaryMap with HashTable
 *)
structure Translate :> TRANSLATE =
struct
  structure H = HashTable
  structure A = Absyn
  structure Frame = MipsFrame
  structure T = Tree
  type level = int *  unit ref
  type access = level * Frame.access
  datatype exp = Ex of Tree.exp
                | Nx of Tree.stm
                | Cx of Temp.label * Temp.label -> T.stm

  val fragList: Frame.frag list ref = ref []
  fun getResult () = !fragList

  exception Translate

  val debug = false
  val k = Frame.k 

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

  fun isRelop(oper) =
    case oper of
       A.EqOp => (true, T.EQ)
       | A.NeqOp => (true, T.NE)
       | A.LtOp => (true, T.LT)
       | A.LeOp => (true, T.LE)
       | A.GtOp => (true, T.GT)
       | A.GeOp => (true, T.GE)
    | (_) => (false, T.EQ)

   fun isBinop(oper) =
      case oper of
         A.PlusOp => (true, T.PLUS)
         | A.MinusOp => (true, T.MINUS)
         | A.TimesOp => (true, T.MUL)
         | A.DivideOp => (true,T.DIV)
      | (_) => (false, T.PLUS)


  fun seq([], a:int) = (print("called with empty" ^ (Int.toString(a))); T.SEQ(T.EXP(T.CONST(0)),T.EXP(T.CONST(0))))
  | seq(stmList, a:int) =
    let
      fun foldStmFn (stm, seqStm) =  T.SEQ (stm,seqStm)
      val count = List.length stmList
    in
      if count = 1 then List.hd(stmList)
      else foldr foldStmFn (List.last stmList) (List.take(stmList,(List.length stmList) -1)) 

        (*foldl foldStmFn [] stmList*)
    end


  fun unEx (Ex e) = e
  | unEx (Cx genstm) =
      let val r = Temp.newNamedTemp("unExResultTemp")
          val t = Temp.newlabel() and f = Temp.newlabel()
      in  T.ESEQ(seq([T.MOVE(T.TEMP r, T.CONST 1),
                     genstm(t,f),
                     T.LABEL f,
                     T.MOVE(T.TEMP r,T.CONST 0),
                     T.LABEL t],1111),
                  T.TEMP r)
      end
  | unEx(Nx s) = T.ESEQ(s,T.CONST 0)

  fun unNx (Ex e) = T.EXP e
    | unNx (Cx genstm) =
        let val l1 = Temp.namedlabel("unNx_Cx")
        in  T.SEQ(genstm(l1,l1),
                 T.LABEL l1)
        end
    | unNx (Nx s) = s

  fun unCx (Cx genstm) = genstm
    | unCx (Ex e) =
        (case e of
          T.CONST 1 =>
            let fun genstm (l1,l2) = T.JUMP(T.NAME l1, [l1])
            in genstm
            end
        | T.CONST 0 =>
            let fun genstm(l1,l2) = T.JUMP(T.NAME l2, [l2])
            in genstm
            end
        | (_) =>
        (let val t = Temp.newlabel()
             val f = Temp.newlabel()
             val z = T.CONST 0
             fun genstm (l1,l2) =

              seq([T.CJUMP(T.EQ,z,e,t,f),
                   T.LABEL t,
                   T.JUMP (T.NAME l1, [l1]),
                   T.LABEL f,
                   T.JUMP (T.NAME l2, [l2])
                  ],1)
        in genstm
        end))
    | unCx (Nx s) =
        let val t = Temp.newlabel()
            (*  should be syntax error to unCx an Nx *)
            fun cxFn (l1,l2) = (T.LABEL l2)
        in
          ErrorMsg.impossible "unCx called on Nx"
          cxFn
        end


  fun debugPrint(msg:string, pos:int) =
    if debug
    then ErrorMsg.error pos msg
    else ()


  fun letExp(decExpList:exp list, body:exp) =
    let
      val nxDecExpList = map(fn x => unNx(x)) decExpList
      val len = List.length decExpList
    in
      case len of 0 => (Ex(unEx body))
      | 1 => Ex(T.ESEQ(hd nxDecExpList, unEx (body)))
      | _ => Ex(T.ESEQ(seq(nxDecExpList,12322),unEx (body)))
    end

  (* move -1 to register as return val *)
  fun nilExp():exp =
    let
      (*val tmp = Temp.newtemp()*)
      val tmp = Temp.newNamedTemp("nilTemp")
    in  Ex(T.ESEQ(T.MOVE(T.TEMP tmp, T.CONST 0), T.TEMP tmp))
    end

  fun intExp(intExp:Absyn.exp) =
    let
        (*val tmp = Temp.newtemp()*)
        val tmp = Temp.newNamedTemp("intExpTemp")
        val const = case intExp of Absyn.IntExp constant => constant
    in  Ex(T.ESEQ(T.MOVE(T.TEMP tmp, T.CONST const),T.TEMP(tmp)))
    end

  fun stringExp(strExp:A.exp) =
      case strExp of
        Absyn.StringExp (lit,_) => (
          let val lab = Temp.newlabel()
          in
            fragList := Frame.STRING(lab, lit) :: !fragList;
            Ex(Tree.NAME(lab))
          end
        )

  fun frameAtLevel (lev:level) =
    let val (levInt, levRef) = lev
    in
        if levInt = 0 then
        outermostFrame
        else H.find frameTable lev
    end

  (* returns exp that calculates static link *)
  fun calcSL (callLev,declaredLev): exp =
    let
      val currentFramePtr = T.TEMP Frame.FP
      val (callLevInt,callLevRef) = callLev
      val (dLevInt,dLevRef) = declaredLev
      val levDiff = callLevInt - dLevInt
      fun getfp(diff, tExp) =
        if diff=0
        then tExp
        else getfp(diff-1, T.MEM (tExp))
    in
      if levDiff > 0
      then Ex(getfp(levDiff, currentFramePtr))
      else (
            if levDiff=0
            then Ex(T.MEM (currentFramePtr))
            else Ex (currentFramePtr)
            )
    end
    
  fun callExp(dLevel:level, cLevel:level, lab:Temp.label, expList:exp list, procedure: bool) =
      (*| CALL of exp * exp list *)
      let
        val slExp = calcSL(cLevel,dLevel)
        val texpList = map(fn x => unEx(x)) expList
      in
        if procedure
        then
          Nx(T.EXP(T.CALL (T.NAME lab, unEx(slExp)::texpList)))
        else
          Nx(T.MOVE(T.TEMP (Temp.newtemp()),T.CALL (T.NAME lab, unEx(slExp)::texpList) ))
      end

  fun strcmp(str1:exp,str2:exp,oper:A.oper,callLevel:level): exp =
    let
      val s1 = unEx str1
      val s2 = unEx str2
    in
      case oper of
          A.EqOp => (
            let
              fun cxFn (t,f) =
                  seq(
                  [T.EXP (T.CALL(T.NAME(Temp.namedlabel("stringEqual")), [unEx (calcSL(callLevel,(0,ref ()))),s1, s2])),
                  T.CJUMP(T.EQ, T.TEMP(Frame.RV), T.CONST 1, t,f)],2
                  )
            in
              Cx cxFn
            end
          )
        | A.NeqOp => (
            let
              fun cxFn (t,f) =
                  seq([
                  T.EXP(T.CALL(T.NAME(Temp.namedlabel("stringEqual")), [unEx (calcSL(callLevel,(0,ref ()))),s1, s2])),
                  T.CJUMP(T.EQ, T.TEMP (Frame.RV), T.CONST 0, t,f)
                  ],3)

            in
              Cx cxFn
            end
          )
        | A.LtOp => (
            let
              fun cxFn (t,f) =
                  seq([
                  T.EXP(T.CALL(T.NAME(Temp.namedlabel("stringCompare")), [unEx (calcSL(callLevel,(0,ref ()))),s1, s2])),
                  T.CJUMP(T.EQ, T.TEMP (Frame.RV), T.CONST (0-1), t,f)
                  ],4)
            in
              Cx cxFn
            end
          )
        | A.LeOp => (
            let
              fun cxFn (t,f) =
                  seq([
                  T.EXP (T.CALL(T.NAME(Temp.namedlabel("stringCompare")), [unEx (calcSL(callLevel,(0,ref ()))),s1, s2])),
                  T.CJUMP(T.LE,T.TEMP (Frame.RV), T.CONST 0, t,f)
                  ],5)
            in
              Cx cxFn
            end
          )
        | A.GtOp => (
            let
              fun cxFn (t,f) =
                  seq([
                  T.EXP (T.CALL(T.NAME(Temp.namedlabel("stringCompare")), [unEx (calcSL(callLevel,(0,ref ()))),s1, s2])),
                  T.CJUMP(T.EQ, T.TEMP (Frame.RV), T.CONST 1, t,f)
                  ],6)
            in
              Cx cxFn
            end
          )
        | A.GeOp => (
            let
              fun cxFn (t,f) =
                  seq([
                  T.EXP (T.CALL(T.NAME(Temp.namedlabel("stringCompare")), [unEx(calcSL(callLevel,(0,ref ()))),s1, s2])),
                  T.CJUMP(T.GE, T.TEMP (Frame.RV), T.CONST 0, t,f)
                  ],7)
            in
              Cx cxFn
            end
          )
      end


  fun opExp(leftExp:exp, rightExp:exp, oper:Absyn.oper) : exp =
    (*unEx leftExp and rightExp
    if oper is binop => binop (Ex)
    if oper is relop => Cx *)
    let
      val lExp = unEx(leftExp)
      val rExp = unEx(rightExp)
      val (relopBoo,relop) = isRelop(oper)
      val (binopBoo,binop) = isBinop(oper)
    in
      if relopBoo
      then
        let fun cxFun (l1,l2) =
              T.CJUMP(relop, lExp, rExp, l1, l2)
        in
            Cx(cxFun)
        end
      else
          Ex(T.BINOP(binop,lExp,rExp))
    end




  (* a=access of var, l=level of function in which var is used *)
  fun simpleVar(a:access, l:level): exp =
    let
        val (dLevInt,fAccess) =  case a of
                                  ((levInt, levRef), fAccess) => (levInt, fAccess)
        val useLevInt = case l of (levInt,levRef) => levInt
        val fpExp = T.TEMP Frame.FP
    in
      if useLevInt = dLevInt
      then Ex(Frame.exp(fAccess)(fpExp))
      else (
          let val levDiff = useLevInt - dLevInt
              fun getfp(diff, tExp) =
                  if diff=0
                  then tExp
                  else getfp(diff-1, T.MEM (tExp))
          in
              Ex(Frame.exp(fAccess)(getfp(levDiff,fpExp)))
          end
        )
    end


  (*| IfExp of {test: exp, then': exp, else': exp option, pos: pos}*)
  fun transIf({test=e1, then'=Nx(i2), else'=SOME(Nx(i3)), pos=_}) =
    let
      val i1 = unCx e1
      val t = Temp.newlabel()
      val f = Temp.newlabel()
      val end' = Temp.newlabel()
      (*val r = Temp.newtemp()*)
      val r = Temp.newNamedTemp("ifExpReturnTemp")
    in
      Ex (T.ESEQ(seq([i1(t,f), T.LABEL(t), T.MOVE(T.TEMP(r), unEx(Nx i2)), T.JUMP(T.NAME(end'), [end']),T.LABEL(f),T.MOVE(T.TEMP(r), unEx(Nx i3)),T.LABEL(end')], 20),T.TEMP(r)))
    end
  | transIf({test=e1, then'=e2, else'=SOME(e3), pos=_}) =
    let
      val i1 = unCx e1
      val i2 = unEx e2
      val i3 = unEx e3
      val t = Temp.newlabel()
      val f = Temp.newlabel()
      val end' = Temp.newlabel()
      (*val r = Temp.newtemp()*)
      val r = Temp.newNamedTemp("ifExpReturnTemp")
    in
      Ex (T.ESEQ(seq([i1(t,f), T.LABEL(t), T.MOVE(T.TEMP(r), i2), T.JUMP(T.NAME(end'), [end']),T.LABEL(f),T.MOVE(T.TEMP(r),i3),T.LABEL(end')],21),T.TEMP(r)))
    end

  | transIf({test=e1, then'=e2, else'=NONE, pos=_}) =
    let val i1 = unCx e1
        val i2 = unNx e2
        val t = Temp.newlabel()
        val f = Temp.newlabel()
    in
        Nx(seq([i1(t,f),T.LABEL t, unNx e2, T.LABEL f],8))
    end


  fun recordExp(sortedFields:(Symbol.symbol * exp) list, cLevel:level) =
    let (*val tmp = Temp.newtemp()*)
        val tmp = Temp.newNamedTemp("recordExpHeadPointerTemp")
        val len = (List.length sortedFields)
        val ctr = ref 0
        val addressExp = T.MOVE(T.TEMP tmp,T.CALL(T.NAME (Temp.namedlabel("malloc")), [unEx (calcSL(cLevel,(0,ref ()))), T.CONST (len*Frame.wordSize)]))
        fun foldExpFn ((s,e),seqExp) =
          if !ctr = len
          then
            seqExp
          else (
            ctr := !ctr+1;
            T.SEQ(seqExp,T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP tmp, T.BINOP(T.MUL,T.CONST (!ctr - 1),T.CONST Frame.wordSize))),unEx e))


          )
        val seqStm = foldl foldExpFn (addressExp) sortedFields
    in
        Ex (T.ESEQ(seqStm,T.TEMP tmp))
    end

  fun arrayExp(sizeExp:exp,initExp:exp,cLevel:level) =
    let val r = Temp.newNamedTemp("mallocReturnAddressTemp")
    in Ex (
        T.ESEQ(seq([T.EXP(T.CALL(
            T.NAME (Temp.namedlabel("initArray")),
            [unEx (calcSL(cLevel,(0,ref ()))),
             T.BINOP(T.PLUS, unEx sizeExp,
             (T.CONST 1)),
             unEx initExp
            ])),
            T.MOVE(T.MEM(T.TEMP Frame.RV),unEx(sizeExp))],3)
            ,T.BINOP(T.PLUS,T.TEMP Frame.RV,T.CONST Frame.wordSize))
       )
    end

  fun assignExp(varExp:exp,assignExp:exp) =
    Nx (T.MOVE(unEx varExp,unEx assignExp))

  fun seqExp(l:exp list) =
    let
      val listLen = List.length l
    in  (
      case listLen of 1 => hd l
      | 0 => (Ex(T.TEMP (Temp.newtemp()))) (* TODO find better noop *)
      | _ =>  let
                val unwrappedList = map (fn x => unNx x) l
                val seqExp = seq(List.take(unwrappedList,List.length(l)-1),21312)
              in
                Ex (T.ESEQ(seqExp,unEx (List.last(l))))
              end
      )

    end
  fun fieldVar(varAccess:exp, fieldOffset:int) =
    Ex (T.MEM(
          T.BINOP(T.PLUS,
                  T.BINOP(T.MUL,
                          T.CONST fieldOffset,
                          T.CONST Frame.wordSize),
                  unEx varAccess
                  )
             )
       )

  fun subscriptVar(varAccess:exp, offsetExp:exp) =
    let val expLabel = Temp.namedlabel("array idx out of bound label")
        val sizeTemp = Temp.newNamedTemp("arraySizeTemp")
        val okLabel = Temp.namedlabel("array idx ok label")
        val doneLabel = Temp.namedlabel("subscriptVar done label")
        val rTemp = Temp.newNamedTemp("subscriptVarResultTemp")
    in (
      (* this IR fragment checks for array out-of-bounds exceptions as well *)
      1+1;
      Ex (
        T.ESEQ(
        seq([
          T.MOVE(T.TEMP(sizeTemp), T.MEM(T.BINOP(T.MINUS, unEx varAccess, T.CONST 4))),
          T.CJUMP(T.GT, T.TEMP(sizeTemp), unEx (offsetExp), okLabel,expLabel),
          T.LABEL(okLabel),
          T.MOVE(T.TEMP rTemp,
            T.MEM(
              T.BINOP(T.PLUS,
                    unEx varAccess,
                    T.BINOP(T.MUL,
                      unEx offsetExp,
                      T.CONST Frame.wordSize
                    )
              )
            )
          ),
          T.JUMP (T.NAME doneLabel, [doneLabel]),
          T.LABEL(expLabel),
          T.EXP( (T.CALL (T.NAME (Temp.namedlabel("exit")), [T.CONST 1]))),
          T.MOVE(T.TEMP rTemp, T.CONST 1),
          T.LABEL(doneLabel)
        ], 3371
        ), T.TEMP rTemp)
      )
    ) end

  fun varDecAlloc(varAccess:access,initExp:exp) =
    let val (level,fAccess) = varAccess
        val frame = case frameAtLevel(level) of
                        SOME(f) => f
                       |NONE => (debugPrint("vardec access does not exist\n",0);valOf outermostFrame)
        val fpExp = T.TEMP Frame.FP
    in
      Nx (T.MOVE(Frame.exp(fAccess)(fpExp), unEx initExp))
    end

  fun procEntryExit({level=funLevel,body=bodyStm}) =
  let val frame = case frameAtLevel(funLevel) of
                        SOME(f) => f
                      | NONE => (debugPrint("fundec level does not exist\n",0); valOf outermostFrame)
  in
    fragList := Frame.PROC({body=bodyStm,frame=frame}) :: !fragList
  end

  fun funDec(funLevel:level, lab:Temp.label, body:exp) =
    let
      val bodyStm = T.MOVE(T.TEMP Frame.RV, unEx body)
    in
      procEntryExit({level=funLevel, body=bodyStm})
    end

  (*| WhileExp of {test: exp, body: exp, pos: pos} *)
  fun transWhile(test, body, donelabel) =
    let
      val l1 = Temp.newlabel()
      val l2 = Temp.newlabel()
    in
      Nx (seq([T.JUMP(T.NAME(l1),[l1]), T.LABEL(l2), unNx body, T.LABEL(l1), unCx test (l2,donelabel), T.LABEL(donelabel)],9))
    end

  fun transError() = Ex (T.CALL (T.NAME (Temp.namedlabel("exit")), [T.CONST 1]))
  fun nilOrUnit() = Ex (T.CONST 0)

  fun transBreak(donelabel) = Nx (T.JUMP(T.NAME(donelabel), [donelabel]))

  fun transFor(loopVar:access, lo, hi, body, donelabel) =
    let
      val frameaccess = case loopVar of (lvl, frameaccess') => frameaccess' (* int * unit ref * frame.access *)
      val loopVar' = Frame.exp frameaccess (T.TEMP Frame.FP) (* need to make sure this is in register *)
      val loopVar'' = Temp.newNamedTemp("for_lo")
      val hi'= Temp.newNamedTemp("for_hi")
      val l1 = Temp.namedlabel("for_true_init_branch")
      val l2 = Temp.namedlabel("for_true_branch")
      val l3 = donelabel
    in
      Nx (seq(
        [ (* in case loopvar' is in MEM. TODO: pattern match would be more efficient *)
        T.MOVE(T.TEMP loopVar'', unEx lo),
        T.MOVE(T.TEMP hi', unEx hi),
        T.CJUMP(T.LE, T.TEMP loopVar'', T.TEMP hi', l2, l3),
        T.LABEL l1,
        T.MOVE(T.TEMP loopVar'', (T.BINOP(T.PLUS, T.TEMP loopVar'', T.CONST 1))),
        T.LABEL l2,
        unNx body,
        T.CJUMP(T.LT, T.TEMP loopVar'', T.TEMP hi', l1, l3),
        T.LABEL l3],10
      ))
    end

  fun refCompare(leftRef:exp, rightRef:exp, oper:Absyn.oper) =
    let
        val leftAdd = case unEx(leftRef) of
                          T.MEM(l) => l
                        | T.TEMP(l) => T.TEMP(l)
        val rightAdd = case unEx(rightRef) of
                          T.MEM(rr) => rr
                        | T.TEMP(rr) => T.TEMP(rr)
                        | T.CONST 0 => T.CONST 0
                        | T.CALL _ => T.CONST ~1 (* if error was thrown, we will get here *)
        val r = Temp.newNamedTemp("refCompResultTemp")
        val t = Temp.newlabel()
        val f = Temp.newlabel()
        val end' = Temp.newlabel()
        val (relop,rop) = isRelop(oper)
        val (binop,bop) = isBinop(oper)
    in
      if binop
      then (
        ErrorMsg.error 0 "[IR] Used binopto compare refs.";
        transError()
      )
      else(
        if relop
        then (
          case rop of (T.EQ | T.NE) =>
            Ex(T.ESEQ(seq([
              T.CJUMP(rop,leftAdd,rightAdd,t,f),
              T.LABEL t,
              T.MOVE(T.TEMP r, T.CONST 1),
              T.JUMP(T.NAME end', [end']),
              T.LABEL f,
              T.MOVE(T.TEMP r,T.CONST 0),
              T.LABEL end'
              ],11),T.TEMP r))
          | (_) => (
            ErrorMsg.error 0 "[IR] Used invalid relop to compare refs.";
            transError()
          )
        )
        else (ErrorMsg.impossible "refcompare saw neither relop or binop"; transError())
      )
    end

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
