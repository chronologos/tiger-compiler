structure MipsFrame :> FRAME =
struct
  datatype access = InFrame of int | InReg of Temp.temp
  type frame = {name:Temp.label, kFormals:access list, moreFormals:access list, fpMaxOffset:int ref}

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
        val access = foldr foldFn [] bools
        val offset = ref 0
    in
        offset := !maxOffset;
        let
            val ret:frame = {name=label, kFormals=access,fpMaxOffset=offset,moreFormals=moreFormals}
        in
            ret
        end
    end

  fun name ({name=label, kFormals=list, fpMaxOffset=offst, moreFormals=moreFormals}) = label

  fun formals ({name=label, kFormals=kFormals, moreFormals=moreFormals, fpMaxOffset=offst}) = kFormals @ moreFormals

  fun allocLocal ({name=label:Temp.label, kFormals=list:access list, moreFormals=moreFormals, fpMaxOffset=offset: int ref}) =
    let
      fun allocLocalBool ecp =
        if ecp
        then (
          offset := !offset-4;
          InFrame(!offset)
        )
        else (
          InReg(Temp.newtemp())
        )
    in
      allocLocalBool
    end
end
