structure MipsFrame :> FRAME =
struct
  structure T = Tree
  structure A = Assem
  type temp = A.temp
  datatype access = InFrame of int | InReg of Temp.temp
  type frame = {name:Temp.label, formals:access list, fpMaxOffset:int ref, procEntryExit1list: Tree.stm list, maxCallArgs: int}

  datatype frag =  PROC of {body:Tree.stm, frame:frame}
                 | STRING of Temp.label * string


  val wordSize = 4
  val k = 4
  val debug = false

  val FP = Temp.newNamedTempTrue("FP")
  val SP = Temp.newNamedTempTrue("SP")
  val RV = Temp.newNamedTempTrue("RV")
  val ZERO = Temp.newNamedTempTrue("ZERO")
  val RA = Temp.newNamedTempTrue("RA")
  val ERROR = Temp.newNamedTemp("ERROR")

  fun initRegs (0,someLetter) = []
  | initRegs(i, someLetter) = initRegs(i-1,someLetter) @ [Temp.newNamedTempTrue(someLetter^Int.toString(i-1))]
    
  fun intToAssemStr(i:int) = 
    if i >= 0 then (Int.toString(i)) 
    else String.implode(hd(String.explode("-")) :: (tl (String.explode(Int.toString(i)))))
    
  val aRegNum = 4
  val sRegNum = 8
  val tRegNum = 10
  val argregs = initRegs(aRegNum, "a")
  val specialregs = [FP,SP,RV,RA,ZERO]
  val calleesaves = initRegs(sRegNum,"s") (* s0 - s7 *)
  val callersaves = initRegs(tRegNum,"t") (* t0 - t9 *)
  val usableRegisters = Temp.Set.addList(Temp.Set.empty, (callersaves @ calleesaves))
  val allRegisters = Temp.Set.addList(Temp.Set.empty, (calleesaves @ callersaves @ specialregs @  argregs))

  val regsMap:Temp.temp StringMap.map = (Temp.Set.foldl (fn (nextReg, mapSofar) =>
                StringMap.insert(mapSofar, Temp.makestring(nextReg), nextReg)) StringMap.empty usableRegisters)
                
  val NUMREG = Temp.Set.numItems(usableRegisters)
  
  fun maxCallArgs({name=_, formals=_, fpMaxOffset=_, procEntryExit1list=_, maxCallArgs=mca}) = mca
  
  fun setCallArgs({name=n, formals=f, fpMaxOffset=fp, procEntryExit1list=l, maxCallArgs=mca}, calls) =
    if calls > mca 
    then {name=n, formals=f, fpMaxOffset=fp, procEntryExit1list=l, maxCallArgs=calls}
    else {name=n, formals=f, fpMaxOffset=fp, procEntryExit1list=l, maxCallArgs=mca}
    
  fun getInitialAlloc(regs, startTable) =
    let fun foldFn(nextReg, tab) = Temp.Map.insert(tab, nextReg, nextReg)
    in
      Temp.Set.foldl foldFn startTable regs
    end

  val regTable = 
    let 
      val t = Temp.Map.empty
    in
      getInitialAlloc(allRegisters, t)
    end
  
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
    
  fun string(label,s) =
     Symbol.name(label) ^ ": .ascii \"" ^ (String.toCString(s)) ^ "\"\n"
      (* 7 implemented by translate *)
  fun procEntryExit1 (frame:frame, body:T.stm ) = 
    (* Save the s registers and RA*)
    let val regsToSave = RA :: FP :: calleesaves
        fun saveFoldFn(nextReg, (pos,stmlist)) =
          let
              val stm = T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP(FP), T.CONST pos)), T.TEMP(nextReg))
          in
            (pos - wordSize, (stm::stmlist))
          end
        
        fun loadFoldFn(nextReg, (pos, stmlist)) = 
          let 
              val stm = T.MOVE(T.TEMP(nextReg),T.MEM(T.BINOP(T.PLUS, T.TEMP(FP), T.CONST pos)))
          in
            (pos - wordSize, (stm::stmlist))
          end
          
        val (_, saveStmList) = foldr saveFoldFn (!(#fpMaxOffset frame), []) regsToSave
        val (_, loadStmList) =  foldr loadFoldFn (!(#fpMaxOffset frame), []) regsToSave
        val stm = seq((#procEntryExit1list frame) @ saveStmList @ (body::loadStmList),123)
    in
        stm
    end
    
    
  (*  
  fun findMaxArgs(funcStm:Tree.stm) =
    case funcStm of (currStm, nextStm) =>
      let fun foldFn(nextStm, maxSoFar) = 
        case nextStm of T.CALL(exp1:Tree.exp, expList:Tree.exp list) =>
          if List.length(expList) > maxSoFar then List.length(expList) else maxSoFar
        | (_) => maxSoFar
  *)      

  fun procEntryExit2 (frame,instrlist) =
    instrlist @ [A.OPER{assem="", src=[ZERO,RA,SP]@calleesaves, dst=[], jump=SOME[]}]
  (* sink liveness *)

  fun procEntryExit3({name=_, formals=_, fpMaxOffset=fpMaxOffset, procEntryExit1list=_, maxCallArgs=maxCallArgs}) =
       
       let 
           val numSavedRegs = List.length(RA::FP::calleesaves)
           val maxFrameSize = maxCallArgs*wordSize - !fpMaxOffset + numSavedRegs * wordSize
           val saveFPStm = Assem.OPER{assem="addi `d0, `s0 " ^ intToAssemStr(~4) ^ "\n", src=[SP], dst=[FP], jump=NONE}
           val changeSPStm = Assem.OPER{assem="addi `d0, `s0, " ^ intToAssemStr(~maxFrameSize) ^ "\n", src=[SP], dst=[SP], jump=NONE} 
            (* epilog *)
           val restoreSP = Assem.OPER{assem="addi `d0, `s0, " ^ intToAssemStr(maxFrameSize) ^ "\n", src=[SP], dst=[SP], jump=NONE}
           val jrRa = Assem.OPER{assem="jr `s0\n", src=[RA], dst=[], jump = SOME[]}
      in
      {prolog = saveFPStm :: changeSPStm :: [],       
       epilog = restoreSP :: jrRa :: []}
      end

  fun debugPrint(msg:string, pos:int) =
    if debug
    then ErrorMsg.error pos msg
    else ()

  fun exp (a) (tExp) =
    case a of
      InFrame(k) =>
        T.MEM(T.BINOP(T.PLUS,tExp,T.CONST(k)))
    | InReg(tmp) =>
        T.TEMP tmp

  fun maxCallArgs (f:frame) = #maxCallArgs f

  fun newFrame(label:Temp.label, formals: bool list) =
  let
    val maxOffset = ref 0
    fun foldFn (boo, (idx,stmList,accessList))= (
      maxOffset := !maxOffset+wordSize;
      (* code to move *)
      if (idx < k) then (
        (* if escapes move from reg to frame *) 
        if List.nth(true::formals,idx) 
          then (
            let val stm = T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST (!maxOffset-wordSize), T.TEMP(FP))), T.TEMP(List.nth(argregs, idx)))
                val access = InFrame((!maxOffset-wordSize)) 
            in
              (idx+1,(stm :: stmList), (access :: accessList))
            end
          )
        else (
          let 
            val newtemp = Temp.newtemp()
            val stm = T.MOVE(T.TEMP(newtemp), T.TEMP (List.nth(argregs,idx)))
            val access = InReg(newtemp) 
          in  
            (idx+1, (stm::stmList), (access::accessList))  
          end
        )
        (* in procEntryExit: if doesnt escape move from aReg to sReg *)
    ) else (
      (* access from parent's frame *)
      (* idx >= k *)
      (idx+1, stmList, InFrame(!maxOffset-wordSize)::accessList)
    )
  )

    val (_, stmList, access) = foldr foldFn (0,[],[]) (true::formals)
    val _ = debugPrint("formal list for function label "^Symbol.name(label)^" \n",0)
    val ptBools = map(fn x => debugPrint(Bool.toString(x),0)) (formals)
  in
    {name=label, formals=access, fpMaxOffset=ref (0-wordSize), procEntryExit1list=stmList, maxCallArgs=0}
  end 

  fun name (f:frame) = #name f
  fun formals (f:frame) =  
    let val list = #formals f 
        val len = List.length list
    in
        if len=0 
        then []
        else List.drop(#formals f,1)
    end

  fun allocLocal ({name=label:Temp.label, formals=formals:access list, fpMaxOffset=offset: int ref, procEntryExit1list: Tree.stm list, maxCallArgs: int}) =
    let
      fun allocLocalBool ecp =
        if ecp
        then (
          offset := !offset-wordSize;
          
          InFrame(!offset+wordSize)
        )
        else (
          InReg(Temp.newNamedTemp("localVarTemp"))
        )
    in
      allocLocalBool
    end
    
end

