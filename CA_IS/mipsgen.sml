structure MipsGen :> CODEGEN = struct
  structure A = Assem and T = Tree and S = Symbol
  structure Frame = MipsFrame
  fun codegen frame stm : A.instr list = 
  let
    val ilist = ref (nil: A.instr list)
    fun emit x = ilist := x :: !ilist
    fun result(gen) = let val t = Temp.newtemp() in gen t; t end
    and fun munchStm(
    
    (* fn calls *) 
    | munchStm(T.EXP(T.CALL(T.NAME l, args:Tree.exp list))) =
      emit(A.OPER{assem="jal " ^ Symbol.name(l) ^ "\n", dst=[Frame.RV], src=[], jump=SOME([l])})
   
    (* save to mem *) 
    | (munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,e1,T.CONST i)),e2)) | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1)),e2)) ) =
      let val munchE1 = munchExp(e1) 
          val munchE2 = munchExp(e2)
      in
        emit(A.OPER{assem="sw `s0, " ^ Int.toString(i) ^ "(`s1)\n",
                    src=[munchE1, munchE2], 
                    dst=[], 
                    jump=NONE})
      end
    
    | munchStm(T.MOVE(T.MEM(T.BINOP(T.MINUS,e1,T.CONST i)),e2)) =
      let val munchE1 = munchExp(e1) 
          val munchE2 = munchExp(e2)
      in
        emit(A.OPER{assem="sw `s0, " ^ Int.toString(~i) ^ "(`s1)\n",
                    src=[munchE1, munchE2], 
                    dst=[], 
                    jump=NONE})
      end 
    
    | munchStm(T.MOVE(T.MEM(e1), T.MEM(e2))) = 
      let 
        val munchE1 = munchExp(e1)
        val munchE2 = munchExp(e2)
        val tmp = Temp.newNamedTemp("Mem to mem intermediate value")
      in 
        emit(A.OPER{assem="lw `d0, 0(`s0)\n",
                     src=[munchE2],
                     dst=[tmp],
                     jump=NONE});
        emit(A.OPER{assem="sw `s0, 0(`s1)\n",
                     src=[tmp, munchE1],
                     dst=[],
                     jump=NONE})
      end
      
    | munchStm(T.MOVE(T.MEM(T.CONST i), e2)) = 
      let
        val munchE2 = munchExp(e2)
        val tmp = Temp.newNamedTemp("mem(const) tmp")
      in
        emit(A.OPER{assem="sw `s0, 0(`s1)\n",
                    src=[munchE2, tmp],
                    dst=[],
                    jump=NONE})
      end
    
    | munchStm(T.MOVE(T.MEM(e1), e2)) = 
      (* store *)
      let val munchE1 = munchExp(e1)
          val munchE2 = munchExp(e2)
      in
        emit(A.OPER{assem="sw `s1, 0(`s0)\n",
                    src=[munchE1, munchE2],
                    dst=[],
                    jump=NONE)
      end
    
    (* constant to reg *)
    | munchStm(T.MOVE(T.TEMP t, T.CONST i)) =
      emit(A.OPER{assem="li `d0, " ^ Int.toString(i) ^ "\n",
                  src=[],
                  dst=[t],
                  jump=NONE})
    
    | munchStm(T.MOVE(T.TEMP t, T.NAME l)) =
      emit(A.OPER{assem="la `d0, " ^ S.name(l) ^ "\n",
                  src=[],
                  dst=[t],
                  jump=NONE})
    (* binops *)
    
    (* jumps and branches *) 
    
    (* other stuff *)
    | munchStm(T.LABEL lab) = 
      emit(A.LABEL{assem=S.name(lab) ^ ":\n", lab=lab})
    | munchStm(T.EXP e) = (munchExp e; ())
    
    (* load from mem should be in munchexp*)
    and munchExp ...
    and fun munchArgs(i:int, args: Tree.exp list) = 
      let 
        fun foldFn(nextExp, listSoFar) = 
          let 
            val len = List.length(listSoFar)
            val nextArgReg = List.nth(Frame.argregs, len)
            in
            case nextExp of T.TEMP(t) =>
              if len >= Frame.k then
                () (* TODO *)
              else
                ()
              emit(assem="MOVE " ^ Temp.getNameFromNamedTemp(nextArgReg) ^ " " ^ Temp.makeString(munchExp(nextExp)), src=[munchExp e1], dst=[nextArgReg], jump = NONE);
              nextArgReg :: listSoFar
            | _ => listSoFar
          end
      in
        foldr foldFn [] args
      end
      
  in 
    munchStm stm;
    List.rev(!ilist) (* maximal munch is top down so need to reverse *)
  end
end
