structure MipsFrame :> FRAME =
struct
  datatype access = InFrame of int | InReg of Temp.temp
  type frame = {name:Temp.label, formals:access list, fpMaxOffset:int ref}

  fun newFrame ({name=label:Temp.label, formals=bools: bool list}) =
    let
        val maxOffset = ref 0
        (* assume all bools are true for simplicity *)
        fun foldFn (boo, accessList) =
          let
            val offset = !maxOffset
          in
            maxOffset := offset-4;
            InFrame(offset)::accessList
          end
        val access = foldr foldFn [] bools
        val offset = ref 0
    in
        offset := !maxOffset;
        let
            val ret:frame = {name=label, formals=access,fpMaxOffset=offset}
        in
            ret
        end
    end

  fun name ({name=label, formals=list, fpMaxOffset=offst}) = label

  fun formals ({name=label, formals=list, fpMaxOffset=offst}) = list

  fun allocLocal ({name=label:Temp.label, formals=list:access list,fpMaxOffset=offset: int ref}) =
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
