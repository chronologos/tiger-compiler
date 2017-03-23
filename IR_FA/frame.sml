structure MipsFrame :> FRAME =
struct
  structure T = Tree
  datatype access = InFrame of int | InReg of Temp.temp
  type frame = {name:Temp.label, kFormals:access list, moreFormals:access list, fpMaxOffset:int ref}
  val FP = Temp.newtemp()
  val SP = Temp.newtemp()
  val wordSize = 4

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
          ) else (
            InReg(Temp.newtemp())::accessList
          )
        fun foldMoreFormals (InFrame(accInt),accessList) =
          case accessList of
              [] => InFrame(wordSize)::[]
            | (InFrame(a)::l) => InFrame(a+wordSize)::accessList

        val access = foldr foldFn [] (true::bools)
        val offset = ref 0
        val moreFormalsOfsetCurrFrame = foldr foldMoreFormals [] moreFormals
    in
        offset := !maxOffset;
        let
            val ret:frame = {name=label, kFormals=access,fpMaxOffset=offset,moreFormals=moreFormalsOfsetCurrFrame}
        in
            ret
        end
    end

  fun name ({name=label, kFormals=list, fpMaxOffset=offst, moreFormals=moreFormals}) = label

  fun formals ({name=label, kFormals=kFormals, moreFormals=moreFormals, fpMaxOffset=offst}) = List.drop(kFormals @ moreFormals , 1)

  fun allocLocal ({name=label:Temp.label, kFormals=list:access list, moreFormals=moreFormals, fpMaxOffset=offset: int ref}) =
    let
      fun allocLocalBool ecp =
        if ecp
        then (
          offset := !offset-wordSize;
          InFrame(!offset)
        )
        else (
          InReg(Temp.newtemp())
        )
    in
      allocLocalBool
    end
end
