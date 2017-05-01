structure MipsFrame :> FRAME =
struct
  structure T = Tree
  structure A = Assem
  type temp = A.temp
  datatype access = InFrame of int | InReg of Temp.temp
  type frame = {name:Temp.label, formals:access list, fpMaxOffset:int ref, saveArgs:Assem.instr list, procEntryExit1list: Assem.instr list, maxCallArgs: int ref}

  datatype frag =  PROC of {body:Tree.stm, frame:frame}
                 | STRING of Temp.label * string


  val wordSize = 4
  val k = 4
  val debug = false

  val FP = Temp.newNamedTempTrue("fp")
  val SP = Temp.newNamedTempTrue("sp")
  val RV = Temp.newNamedTempTrue("v0")
  val ZERO = Temp.newNamedTempTrue("zero")
  val RA = Temp.newNamedTempTrue("ra")
  val ERROR = Temp.newNamedTemp("ERROR")
  fun getMaxOffset(f:frame) = !(#fpMaxOffset f )
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
  val usableRegisters = Temp.Set.addList(Temp.Set.empty, (callersaves @ calleesaves @ argregs))
  val allRegisters = Temp.Set.addList(Temp.Set.empty, (calleesaves @ callersaves @ specialregs @  argregs))

  val regsMap:Temp.temp StringMap.map = (Temp.Set.foldl (fn (nextReg, mapSofar) =>
                StringMap.insert(mapSofar, Temp.makestring(nextReg), nextReg)) StringMap.empty usableRegisters)
                
  val NUMREG = Temp.Set.numItems(usableRegisters)
  
  fun maxCallArgs({name=_, formals=_, fpMaxOffset=_, procEntryExit1list=_, maxCallArgs=mca}) = !mca
  
  fun setCallArgs({name=n, formals=f, fpMaxOffset=fp,saveArgs=saveArgs, procEntryExit1list=l, maxCallArgs=mca}, calls) =
    ( 
    if calls > !mca 
    then mca := calls
    else ()
    )
    
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
    
  fun procEntryExit4(frame:frame) = #saveArgs frame
     
  fun procEntryExit1(frame:frame) =
    let 
        val t0 = List.nth(callersaves,0)
        val regsToSave = RA :: calleesaves @ [t0]
        
        fun saveFoldFn(nextReg, (pos,stmlist)) =
          let
            val stm = A.OPER{assem="sw `s0, " ^  intToAssemStr(pos) ^ "(`d0)\n", src=[nextReg], dst=[FP], jump=NONE}
          in
            (pos - wordSize, (stm::stmlist))
          end
          
        fun loadFoldFn(nextReg, (pos, stmlist)) = 
            let
              val stm = A.OPER{assem="lw `d0, " ^ intToAssemStr(pos) ^ "(`s0)\n", src=[FP], dst=[nextReg], jump=NONE}
            in
               (pos - wordSize, (stm::stmlist))
            end 
        val (_, saveStmList) = foldr saveFoldFn (!(#fpMaxOffset frame), []) regsToSave;
        val (_, loadStmList) =  foldr loadFoldFn (!(#fpMaxOffset frame), []) regsToSave 
    in (
        {params=(#procEntryExit1list frame), saves=saveStmList, loads=loadStmList}
        )
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

  fun procEntryExit3({name=_, formals=_, fpMaxOffset=fpMaxOffset,saveArgs=_, procEntryExit1list=_, maxCallArgs=maxCallArgs}) =
       
       let
          val t0 = List.nth(callersaves, 0)
          val moveFPt0 = A.OPER{assem="move `d0, `s0\n", src=[FP], dst=[t0], jump=NONE}

           val numSavedRegs = List.length(RA::FP::calleesaves)
           val maxFrameSize = !maxCallArgs*wordSize - !fpMaxOffset + numSavedRegs * wordSize
          
           val saveFPStm = Assem.OPER{assem="addi `d0, `s0, " ^ intToAssemStr(~4) ^ "\n", src=[SP], dst=[FP], jump=NONE}
           val changeSPStm = Assem.OPER{assem="addi `d0, `s0, " ^ intToAssemStr(~maxFrameSize) ^ "\n", src=[SP], dst=[SP], jump=NONE} 
            (* epilog *)
           
           val restoreSP = Assem.OPER{assem="addi `d0, `s0, " ^ intToAssemStr(maxFrameSize) ^ "\n", src=[SP], dst=[SP], jump=NONE}
           (*val restoreFP = Assem.OPER{assem="addi `d0, `s0, " ^ intToAssemStr(4) ^ "\n", src=[SP], dst=[FP], jump = NONE}*)
           
           
           val movet0FP = A.OPER{assem="move `d0, `s0\n", src=[t0], dst=[FP], jump=NONE}
           val jrRa = Assem.OPER{assem="jr `s0\n", src=[RA], dst=[], jump = SOME[]}
      in
     
      {prolog = moveFPt0 :: saveFPStm :: changeSPStm :: [],       
       epilog = restoreSP  :: movet0FP :: jrRa :: []}
       
      (* {prolog = saveFPStm :: changeSPStm :: [],
       { prolog1 = , 
       prolog2 = ,
       epilog1 = restoreSP :: restoreFP :: [],
       epilog2 = [jrRa]}
      *)
       
      end
      
  fun debugPrint(msg:string, pos:int) =
    if debug
    then ErrorMsg.error pos msg
    else ()

  fun exp (a) (tExp) =
    case a of
      InFrame(k) =>(
        T.MEM(T.BINOP(T.PLUS,tExp,T.CONST(k)))
      )
    | InReg(tmp) =>
      (
        T.TEMP tmp
        )

  fun maxCallArgs (f:frame) = !(#maxCallArgs f)
  fun printAccessList(m:string,l:access list) =
   (
   print("MODULE "^m^"\n");
   app(fn (a) => case a of 
                  InFrame(k) => print(m^" inFrame("^Int.toString(k)^"),")
                  |InReg(k) => print(m^" inReg("^Temp.makestring(k)^"),")
                  ) l;
   print("\n")
   )

  fun newFrame(label:Temp.label, formals: bool list) =
  let
    
    val maxOffset = ref 0
    fun foldFn (boo, (idx,saveArgs,stmList,accessList))= (
      maxOffset := !maxOffset+wordSize;
      (* code to move *)
      if (idx < k) then (
        print(Bool.toString(List.nth(true::formals,idx))^"\n");
        (* if escapes move from reg to frame *) 
        if List.nth((true::formals),idx) = true 
          then (
            let 
                (*val stm = T.MOVE(T.MEM
                                          (T.BINOP(T.PLUS, T.CONST (!maxOffset+wordSize), T.TEMP(FP))), 
                                          T.TEMP(List.nth(argregs, idx)))*)
                val fpOffset = !maxOffset-wordSize
                val argReg = List.nth(argregs, idx)
                val stm = A.OPER{assem="sw `s0, "^intToAssemStr(fpOffset)^"(`d0)\n",src=[argReg], dst=[FP],jump=NONE}
                val access = InFrame((!maxOffset-wordSize)) 
            in
              (*printAccessList("",(accessList @ [access]));*)
              (idx+1,saveArgs,(stmList @ [stm]), (accessList @ [access]))
            end
          )
        else (
          let 
            val newtemp = Temp.newtemp()
            val stmSaveArg = A.OPER{assem="move `d0, `s0\n", src=[List.nth(argregs,idx)], dst = [newtemp], jump=NONE}
            val access = InReg(newtemp) 
          in  
            (idx+1,(saveArgs@[stmSaveArg]), stmList, (accessList@[access]))  
          end
        )
        (* in procEntryExit: if doesnt escape move from aReg to sReg *)
    ) else (
      (* access from parent's frame *)
      (* idx >= k *)
      (idx+1, saveArgs , stmList, accessList@[InFrame(!maxOffset-wordSize)])
    )
  )

    val (_,saveArgs, stmList, access) = foldl foldFn (0,[],[],[]) ([true]@formals)
    val _ = debugPrint("formal list for function label "^Symbol.name(label)^" \n",0)
    val ptBools = map(fn x => debugPrint(Bool.toString(x),0)) (formals)
  in
  (
    (*printAccessList("Frame.newFrame",access);*)
    (*app(fn (a) => print(Bool.toString(a)^",")
                    ) (true::formals);
    print("\n");*)
    {name=label, formals=access, fpMaxOffset=ref (~4), saveArgs=saveArgs, procEntryExit1list=stmList, maxCallArgs=ref k}
  )
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

  fun allocLocal ({name=label:Temp.label, saveArgs=saveArgs, formals=formals:access list, fpMaxOffset=offset: int ref, procEntryExit1list: Assem.instr list, maxCallArgs: int ref}) =
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

structure MipsFrame :> FRAME =
struct
  structure T = Tree
  structure A = Assem
  type temp = A.temp
  datatype access = InFrame of int | InReg of Temp.temp
  type frame = {name:Temp.label, formals:access list, fpMaxOffset:int ref, saveArgs:Assem.instr list, procEntryExit1list: Assem.instr list, maxCallArgs: int ref}

  datatype frag =  PROC of {body:Tree.stm, frame:frame}
                 | STRING of Temp.label * string


  val wordSize = 4
  val k = 4
  val debug = false

  val FP = Temp.newNamedTempTrue("fp")
  val SP = Temp.newNamedTempTrue("sp")
  val RV = Temp.newNamedTempTrue("v0")
  val ZERO = Temp.newNamedTempTrue("zero")
  val RA = Temp.newNamedTempTrue("ra")
  val ERROR = Temp.newNamedTemp("ERROR")
  fun getMaxOffset(f:frame) = !(#fpMaxOffset f )
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
  val usableRegisters = Temp.Set.addList(Temp.Set.empty, (callersaves @ calleesaves @ argregs))
  val allRegisters = Temp.Set.addList(Temp.Set.empty, (calleesaves @ callersaves @ specialregs @  argregs))

  val regsMap:Temp.temp StringMap.map = (Temp.Set.foldl (fn (nextReg, mapSofar) =>
                StringMap.insert(mapSofar, Temp.makestring(nextReg), nextReg)) StringMap.empty usableRegisters)
                
  val NUMREG = Temp.Set.numItems(usableRegisters)
  
  fun maxCallArgs({name=_, formals=_, fpMaxOffset=_, procEntryExit1list=_, maxCallArgs=mca}) = !mca
  
  fun setCallArgs({name=n, formals=f, fpMaxOffset=fp,saveArgs=saveArgs, procEntryExit1list=l, maxCallArgs=mca}, calls) =
    ( 
    if calls > !mca 
    then mca := calls
    else ()
    )
    
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
    
  fun procEntryExit4(frame:frame) = #saveArgs frame
     
  fun procEntryExit1(frame:frame) =
    let 
        val t0 = List.nth(callersaves,0)
        val regsToSave = RA :: calleesaves @ [t0]
        
        fun saveFoldFn(nextReg, (pos,stmlist)) =
          let
            val stm = A.OPER{assem="sw `s0, " ^  intToAssemStr(pos) ^ "(`d0)\n", src=[nextReg], dst=[FP], jump=NONE}
          in
            (pos - wordSize, (stm::stmlist))
          end
          
        fun loadFoldFn(nextReg, (pos, stmlist)) = 
            let
              val stm = A.OPER{assem="lw `d0, " ^ intToAssemStr(pos) ^ "(`s0)\n", src=[FP], dst=[nextReg], jump=NONE}
            in
               (pos - wordSize, (stm::stmlist))
            end 
        val (_, saveStmList) = foldr saveFoldFn (!(#fpMaxOffset frame), []) regsToSave;
        val (_, loadStmList) =  foldr loadFoldFn (!(#fpMaxOffset frame), []) regsToSave 
    in (
        {params=(#procEntryExit1list frame), saves=saveStmList, loads=loadStmList}
        )
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

  fun procEntryExit3({name=_, formals=_, fpMaxOffset=fpMaxOffset,saveArgs=_, procEntryExit1list=_, maxCallArgs=maxCallArgs}) =
       
       let
          val t0 = List.nth(callersaves, 0)
          val moveFPt0 = A.OPER{assem="move `d0, `s0\n", src=[FP], dst=[t0], jump=NONE}

           val numSavedRegs = List.length(RA::FP::calleesaves)
           val maxFrameSize = !maxCallArgs*wordSize - !fpMaxOffset + numSavedRegs * wordSize

           val saveFPStm = Assem.OPER{assem="addi `d0, `s0, " ^ intToAssemStr(~4) ^ "\n", src=[SP], dst=[FP], jump=NONE}
           val changeSPStm = Assem.OPER{assem="addi `d0, `s0, " ^ intToAssemStr(~maxFrameSize) ^ "\n", src=[SP], dst=[SP], jump=NONE} 
            (* epilog *)
           
           val restoreSP = Assem.OPER{assem="addi `d0, `s0, " ^ intToAssemStr(maxFrameSize) ^ "\n", src=[SP], dst=[SP], jump=NONE}
           (*val restoreFP = Assem.OPER{assem="addi `d0, `s0, " ^ intToAssemStr(4) ^ "\n", src=[SP], dst=[FP], jump = NONE}*)
           
           
           val movet0FP = A.OPER{assem="move `d0, `s0\n", src=[t0], dst=[FP], jump=NONE}
           val jrRa = Assem.OPER{assem="jr `s0\n", src=[RA], dst=[], jump = SOME[]}
      in
     
      {prolog = moveFPt0 :: saveFPStm :: changeSPStm :: [],       
       epilog = restoreSP  :: movet0FP :: jrRa :: []}
       
      (* {prolog = saveFPStm :: changeSPStm :: [],
       { prolog1 = , 
       prolog2 = ,
       epilog1 = restoreSP :: restoreFP :: [],
       epilog2 = [jrRa]}
      *)
       
      end
      
  fun debugPrint(msg:string, pos:int) =
    if debug
    then ErrorMsg.error pos msg
    else ()

  fun exp (a) (tExp) =
    case a of
      InFrame(k) =>(
        T.MEM(T.BINOP(T.PLUS,tExp,T.CONST(k)))
      )
    | InReg(tmp) =>
      (
        T.TEMP tmp
        )

  fun maxCallArgs (f:frame) = !(#maxCallArgs f)
  fun printAccessList(m:string,l:access list) =
   (
   print("MODULE "^m^"\n");
   app(fn (a) => case a of 
                  InFrame(k) => print(m^" inFrame("^Int.toString(k)^"),")
                  |InReg(k) => print(m^" inReg("^Temp.makestring(k)^"),")
                  ) l;
   print("\n")
   )

  fun newFrame(label:Temp.label, formals: bool list) =
  let
    
    val maxOffset = ref 0
    fun foldFn (boo, (idx,saveArgs,stmList,accessList))= (
      maxOffset := !maxOffset+wordSize;
      (* code to move *)
      if (idx < k) then (
        (* if escapes move from reg to frame *) 
        if List.nth((true::formals),idx) = true 
          then (
            let 
                (*val stm = T.MOVE(T.MEM
                                          (T.BINOP(T.PLUS, T.CONST (!maxOffset+wordSize), T.TEMP(FP))), 
                                          T.TEMP(List.nth(argregs, idx)))*)
                val fpOffset = !maxOffset-wordSize
                val argReg = List.nth(argregs, idx)
                val stm = A.OPER{assem="sw `s0, "^intToAssemStr(fpOffset)^"(`d0)\n",src=[argReg], dst=[FP],jump=NONE}
                val access = InFrame((!maxOffset-wordSize)) 
            in
              (*printAccessList("",(accessList @ [access]));*)
              (idx+1,saveArgs,(stmList @ [stm]), (accessList @ [access]))
            end
          )
        else (
          let 
            val newtemp = Temp.newtemp()
            val stmSaveArg = A.OPER{assem="move `d0, `s0\n", src=[List.nth(argregs,idx)], dst = [newtemp], jump=NONE}
            val access = InReg(newtemp) 
          in  
            (idx+1,(saveArgs@[stmSaveArg]), stmList, (accessList@[access]))  
          end
        )
        (* in procEntryExit: if doesnt escape move from aReg to sReg *)
    ) else (
      (* access from parent's frame *)
      (* idx >= k *)
      (idx+1, saveArgs , stmList, accessList@[InFrame(!maxOffset-wordSize)])
    )
  )

    val (_,saveArgs, stmList, access) = foldl foldFn (0,[],[],[]) ([true]@formals)
    val _ = debugPrint("formal list for function label "^Symbol.name(label)^" \n",0)
    val ptBools = map(fn x => debugPrint(Bool.toString(x),0)) (formals)
  in
  (
    (*printAccessList("Frame.newFrame",access);*)
    (*app(fn (a) => print(Bool.toString(a)^",")
                    ) (true::formals);
    print("\n");*)
    {name=label, formals=access, fpMaxOffset=ref (~4), saveArgs=saveArgs, procEntryExit1list=stmList, maxCallArgs=ref k}
  )
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

  fun allocLocal ({name=label:Temp.label, saveArgs=saveArgs, formals=formals:access list, fpMaxOffset=offset: int ref, procEntryExit1list: Assem.instr list, maxCallArgs: int ref}) =
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

