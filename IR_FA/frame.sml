structure MipsFrame :> FRAME =
struct
  type localVarTable = Frame.access Symbol.table
  type frame = {name:Temp.label, formals:access list, fpMaxOffset:int ref}
  type access = InFrame of int | InReg of Temp.temp

  fun newFrame ({name=label, formals=bools}) =
    let
        val maxOffset = ref 0
        (* assume all bools are true for simplicity *)
        fun foldFn (boo, accessList) =
          let
            val offset = !maxOffset
          in
            maxOffset := offset-4;
            offset::accessList
          end
        val access = foldr foldFn [] bools
        val offset = ref 0
        val offset := !maxOffset
    in
        {name=label, formals=access,fpMaxOffset=offset}
    end

  fun name ({name=label, formals=list, fpMaxOffset=offst}) = name

  fun formals ({name=label, formals=list,fpMaxOffset=offst}) = list

  fun allocLocal (){name=label, formals=list,fpMaxOffset=offset}) =
    let
      fun allocLocalBool ecp =
        if ecp
        then (
          offset := !offset-4;
          !offset
        )
        else (
          Temp.newtemp()
        )
    in
      allocLocalBool
    end
end
