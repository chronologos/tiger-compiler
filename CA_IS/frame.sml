structure MipsFrame :> FRAME =
struct
  structure T = Tree
  structure A = Assem
  datatype access = InFrame of int | InReg of Temp.temp

  type frame = {name:Temp.label, kFormals:access list, moreFormals:access list, fpMaxOffset:int ref}

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

  fun initRegs (0,someLetter) = []
  | initRegs(i, someLetter) = initRegs(i-1,someLetter) @ [Temp.newNamedTempTrue(someLetter^Int.toString(i-1))]

  val aRegNum = 4
  val sRegNum = 8
  val tRegNum = 10
  val argregs = initRegs(aRegNum, "a")
  val specialregs = [FP,SP,RV,RA,ZERO]
  val calleesaves = initRegs(sRegNum,"s") (* s0 - s7 *)
  val callersaves = initRegs(tRegNum,"t") (* t0 - t9 *)
  

  fun string(label,s) =
     Symbol.name(label) ^ ": .ascii \"" ^ (String.toCString(s)) ^ "\"\n"
      (* 7 implemented by translate *)
  fun procEntryExit1 (frame,body) = body (* 4 5 8 *)

  fun procEntryExit2 (frame,instrlist) =
    instrlist @ [A.OPER{assem="", src=[ZERO,RA,SP]@calleesaves, dst=[], jump=SOME[]}] (* TODO why is this SOME *)
  (* sink liveness *)

  fun procEntryExit3(frame':{name:Temp.label, kFormals:access list, moreFormals:access list, fpMaxOffset:int ref},body) =
      {prolog = "PROCEDURE "^Symbol.name(#name frame') ^ " \n",
       body = body,
       epilog = "END " ^ Symbol.name (#name frame') ^ " \n"}


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

  fun newFrame ({name=label:Temp.label, kFormals=bools: bool list, moreFormals=moreFormals:access list}) =
    let
        val maxOffset = ref 0
        (* assume all bools are true for simplicity *)
        fun foldFn (boo, accessList) =
          if boo=true
          then (
            let
              val offset = !maxOffset
            in
              maxOffset := offset-4;
              InFrame(offset)::accessList
            end
          ) else InReg(Temp.newNamedTemp("functionParamTemp"))::accessList
        fun foldMoreFormals (InFrame(accInt),accessList) =
          case accessList of
              [] => InFrame(wordSize)::[]
            | (InFrame(a)::l) => InFrame(a+wordSize)::accessList

        val access = foldr foldFn [] (true::bools)
        val offset = ref 0
        val dum = debugPrint("formal list for function label "^Symbol.name(label)^" \n",0)
        val ptBools = map(fn x => debugPrint(Bool.toString(x),0)) (true::bools)
        val moreFormalsOfsetCurrFrame = foldr foldMoreFormals [] moreFormals
    in
        offset := !maxOffset;
        let
            val ret:frame = {name=label, kFormals=access,fpMaxOffset=offset,moreFormals=moreFormalsOfsetCurrFrame}
        in
            ret
        end
    end

  fun name (f:frame) = #name f
  fun formals (f:frame) = List.drop((#kFormals f) @ (#moreFormals f), 1)


  fun allocLocal ({name=label:Temp.label, kFormals=list:access list, moreFormals=moreFormals, fpMaxOffset=offset: int ref}) =
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
