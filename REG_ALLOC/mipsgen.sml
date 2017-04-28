structure MipsGen :> CODEGEN = struct
  structure A = Assem and T = Tree and S = Symbol
  structure Frame = MipsFrame
  fun codegen (frame:MipsFrame.frame) (stm) : A.instr list =
  let
    val ilist = ref (nil: A.instr list)
    fun emit x = ilist := x :: !ilist
    fun result(tempName,gen) = let val t = Temp.newNamedTemp(tempName) in gen t; t end

    fun binopToString(T.PLUS) = "add"
    | binopToString(T.MINUS) = "sub"
    | binopToString(T.DIV) = "div"
    | binopToString(T.MUL) = "mult"
    | binopToString(T.AND) = "and"
    | binopToString(T.OR) = "or"
    | binopToString(T.LSHIFT) = "sll"
    | binopToString(T.RSHIFT) = "srl"
    | binopToString(T.ARSHIFT) = "sra"
    | binopToString(T.XOR) = "xor"


    (* EQ | NE | LT | GT | LE | GE
	        | ULT | ULE | UGT | UGE
    *)

    (*
    fun relopToSring(T.EQ) = "beq"
    | relopToString(T.NE) = "bne"
    | relopToString()
    *)
    
    fun intToAssemStr(i:int) = 
      if i >= 0 then (Int.toString(i)) 
      else String.implode(hd(String.explode("-")) :: (tl (String.explode(Int.toString(i)))))
      
    fun munchArgs(i:int, args: Tree.exp list) =

      let
      (*  val args = List.drop(args, 1) (* drop static link *) *)
        val (_) = print("munchargs len is " ^ Int.toString(List.length(args)) ^ "\n")
        (* according to MIPS calling convention we ALWAYS save at least k word sizes of space in caller frame *)
        val numStackArgs = if List.length(args) > (Frame.k + 1) then (List.length(args)) else (Frame.k + 1)
        val stackArgs = if numStackArgs > (Frame.k + 1) then List.drop(args,Frame.k + 1) else []

        fun shrinkStack(num) =
          munchStm(T.MOVE(T.TEMP(Frame.SP),
                   T.BINOP(T.PLUS,T.CONST(Frame.wordSize * num), T.TEMP(Frame.SP))))
        fun growStack(num) =
          munchStm(T.MOVE(T.TEMP(Frame.SP),
                   T.BINOP(T.MINUS, T.TEMP(Frame.SP), T.CONST(Frame.wordSize * num))))
        fun pushToStack(num, argExp)=
          munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,T.TEMP(Frame.SP),T.CONST(Frame.wordSize * num))), argExp))

        fun pushArgsFoldFn(nextArg, pushedSoFar) = (
          pushToStack(pushedSoFar, nextArg); pushedSoFar + 1 )

        val aRegArgExpTuple =  (ListPair.zip (Frame.argregs, args))
        val aRegsUsed = (map (fn(nextArg, e1) => (
                                    emit(
                                      A.MOVE{assem="move `d0,`s0 \n",
                                           dst=nextArg,
                                           src=munchExp(e1)}
                                    ); nextArg)) aRegArgExpTuple)
        
      in
      (*  growStack(numStackArgs); *)
        foldr pushArgsFoldFn Frame.k stackArgs; (* NOTE : TRANSLATE SAVES ARG 0 CLOSEST TO SP, THIS IS CONSISTENT WITH TRANSLATE*)
        aRegsUsed
      end

    (* fn calls *)
    and munchStm(T.EXP(T.CALL(T.NAME l, args:Tree.exp list))) =
      let val munchArgList = munchArgs(0, args)
      in
        emit(A.OPER{assem="jal " ^ Symbol.name(l) ^ "\n", dst=(Frame.RV::Frame.RA::Frame.callersaves @ Frame.argregs), src=munchArgList, jump=NONE})
      end

    (* save to mem *)
    | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,e1,T.CONST i)),e2)) =
      let val munchE1 = munchExp(e1)
          val munchE2 = munchExp(e2)
      in
        emit(A.OPER{assem="sw `s1, " ^ intToAssemStr(i) ^ "(`s0)\n",
                    src=[munchE1, munchE2],
                    dst=[],
                    jump=NONE})
      end

    | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1)),e2))   =
      let val munchE1 = munchExp(e1)
          val munchE2 = munchExp(e2)
      in
        emit(A.OPER{assem="sw `s1, " ^ intToAssemStr(i) ^ "(`s0)\n",
                    src=[munchE1, munchE2],
                    dst=[],
                    jump=NONE})
      end

    | munchStm(T.MOVE(T.MEM(T.BINOP(T.MINUS,e1,T.CONST i)),e2)) =
      let val munchE1 = munchExp(e1)
          val munchE2 = munchExp(e2)
      in
        emit(A.OPER{assem="sw `s1, " ^ intToAssemStr(~i) ^ "(`s0)\n",
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
                    jump=NONE})
      end




    | munchStm(T.MOVE(e1,T.TEMP t)) =
      let val munchE1 = munchExp(e1)
      in
        emit(
        (*A.OPER{assem="move `d0, $zero, `s0 \n", src=[t], dst=[munchE1],jump=NONE}*)
        A.MOVE{assem= "move `d0, `s0 \n",
                            dst=munchE1,
                            src=t}
        )
      end

                  
    (* constant to reg *)
    | munchStm(T.MOVE(T.TEMP t, T.CONST i)) =
      emit(A.OPER{assem="li `d0, " ^ intToAssemStr(i) ^ "\n",
                  src=[],
                  dst=[t],
                  jump=NONE})

    | munchStm(T.MOVE(T.TEMP t, T.NAME l)) =
      emit(A.OPER{assem="la `d0, " ^ S.name(l) ^ "\n",
                  src=[],
                  dst=[t],
                  jump=NONE})

    | munchStm(T.MOVE(T.TEMP tmp,e2)) =
      let val munchE2 = munchExp(e2)
      in
        emit(
          (*A.OPER{assem="move `d0, $zero, `s0 \n", src=[munchE2], dst=[tmp],jump=NONE}*)
          A.MOVE {assem= "move `d0, `s0 \n",
                            dst=tmp,
                            src=munchE2}
        )
      end

    | munchStm(T.MOVE(e1,e2)) =
      let val munchE1 = munchExp(e1)
          val munchE2 = munchExp(e2)
      in
          emit(
            (*A.OPER{assem="move `d0, $zero, `s0 \n", src = [munchE2], dst = [munchE1], jump = NONE}*)
            A.MOVE{assem= "move `d0, `s0 \n",
                            dst=munchE1,
                            src=munchE2}
          )
      end

    (* jumps and branches *)
    | munchStm(T.JUMP(T.NAME lab, labList)) =
      emit(A.OPER{assem="j " ^ S.name(lab) ^ "\n",
                  src=[],
                  dst=[],
                  jump=SOME(labList)})

    (* cjumps are  EQ->BEQ | NE->BNE | LT -> BLTZ | GT -> BGTZ | LE -> BLEZ | GE -> BGEZ *)
    | munchStm(T.CJUMP(relop, e1, e2, label1, label2)) =
      let
        val munchE1 = munchExp(e1)
        val munchE2 = munchExp(e2)
        val compTemp = Temp.newNamedTemp("comp_diff")
      in
          case relop of
            T.EQ => emit(A.OPER{assem="beq `s0, `s1, `j0 \n", src=[munchE1, munchE2], dst=[], jump=SOME([label1, label2])})
            | T.NE => emit(A.OPER{assem="bne `s0, `s1, " ^ S.name(label1) ^ "\n",src=[munchE1,munchE2],dst=[],jump=SOME([label1, label2])})
            | T.LT =>  (emit(A.OPER{assem="sub `d0,`s0,`s1 \n", src=[munchE1,munchE2], dst=[compTemp], jump=NONE});
                       emit(A.OPER{assem="bltz `s0, " ^ S.name(label1)^"\n",src=[compTemp],dst=[],jump=SOME([label1, label2])}))
            | T.GE => (emit(A.OPER{assem="sub `d0, `s0, `s1 \n",src=[munchE1, munchE2],dst=[compTemp],jump=NONE});
                       emit(A.OPER{assem="bgez `s0, " ^ S.name(label1) ^ "\n",src=[compTemp],dst=[],jump=SOME([label1, label2])}))
            | T.LE => (emit(A.OPER{assem="sub `d0, `s0, `s1 \n",src=[munchE1,munchE2],dst=[compTemp],jump=NONE});
                        emit(A.OPER{assem="blez `s0, "^S.name(label1) ^ "\n",src=[compTemp],dst=[],jump=SOME([label1, label2])}))
            | T.GT => (emit(A.OPER{assem="sub `d0, `s0, `s1 \n",src=[munchE1, munchE2],dst=[compTemp],jump=NONE});
                      emit(A.OPER{assem="bgtz `s0, " ^  S.name(label1) ^ "\n",src=[compTemp],dst=[],jump=SOME([label1, label2])}))
      end

    (* TODO
    | munchStm(T.JUMP(T.TEMP t, labList)) =
      emit(A.OPER{assem="jr `s0", ""})
    *)

    | munchStm(T.JUMP(_, _)) =
      (ErrorMsg.error 0 "Illegal jump instruction, only jump to label so far";
      emit(A.OPER{assem="jr `s0 \n",src=[],dst=[], jump = NONE}) (* TEMP PLACEHOLDER *))

    (* other stuff *)
    | munchStm(T.LABEL lab) =
      emit(A.LABEL{assem=S.name(lab) ^ ":\n", lab=lab})

    | munchStm(T.EXP e) = (munchExp e; ())
  (*  | munchStm(stm) = (ErrorMsg.error 0 Tree.) *)
  (*         val _ = Printtree.printtree(out,body); *)

(*    | munchStm(x) =
      let val out = TextIO.stdOut
      in
        Printtree.printtree(out, x)
      end
*)
    (* loads from mem *)
    and munchExp(T.MEM(T.BINOP(T.PLUS,e1,T.CONST i))) =
      let
        val munchE1 = munchExp(e1)
      in
        result("T.MEM(T.BINOP(T.PLUS,e1,T.CONST i)",fn r => emit(A.OPER{assem="lw `d0, " ^ intToAssemStr(i) ^ "(`s0)\n", src = [munchE1], dst=[r], jump = NONE}))
      end
    | munchExp(T.MEM(T.BINOP(T.PLUS,T.CONST i,e1))) =
        let
          val munchE1 = munchExp(e1)
        in
          result("T.MEM(T.BINOP(T.PLUS,T.CONST i,e1)",fn r => emit(A.OPER{assem="lw `d0, " ^ intToAssemStr(i) ^ "(`s0)\n", src = [munchE1], dst=[r], jump = NONE}))
        end

    | munchExp(T.MEM(T.CONST i)) =
      (
        ErrorMsg.error 0 "MEM[CONST] \n";
        result("T.MEM(T.CONST i)",fn r => emit(A.OPER{assem="addi `d0, `s0 " ^ intToAssemStr(i),src=[r],dst=[r],jump=NONE}))
      )
    | munchExp(T.MEM(e1)) =
      result("T.MEM(e1)",fn r => emit(A.OPER{assem="lw `d0, 0(`s0)\n", src=[munchExp e1], dst=[r], jump=NONE}))

    (* BINOPS *)
    | munchExp (T.BINOP(T.PLUS, e1, T.CONST i)) =
    let val s = munchExp(e1)
    in
      result("T.BINOP(T.PLUS, e1, T.CONST i)", fn r => emit(A.OPER{assem="addi `d0, `s0, " ^ intToAssemStr(i) ^ "\n",
                  src=[s],
                  dst=[r],
                  jump=NONE}))
    end

    | munchExp (T.BINOP(T.PLUS, T.CONST i, e1)) =
    let val s = munchExp(e1)
    in
      result("T.BINOP(T.PLUS, T.CONST i, e1)",fn r => emit(A.OPER{assem="addi `d0, `s0, " ^ intToAssemStr(i) ^ "\n",
                  src=[s],
                  dst=[r],
                  jump=NONE}))
    end

    | munchExp (T.BINOP(T.XOR, T.CONST i, e1)) =
    let val s = munchExp(e1)
    in
      result("T.BINOP(T.XOR, T.CONST i, e1)",fn r => emit(A.OPER{assem="xori `d0, `s0, " ^ intToAssemStr(i) ^ "\n",
                  src=[s],
                  dst=[r],
                  jump=NONE}))
    end

    | munchExp (T.BINOP(T.XOR, e1, T.CONST i)) =
    let val s = munchExp(e1)
    in
      result("T.BINOP(T.XOR, e1, T.CONST i)",fn r => emit(A.OPER{assem="xori `d0, `s0, " ^ intToAssemStr(i) ^ "\n",
                  src=[s],
                  dst=[r],
                  jump=NONE}))
    end

    | munchExp (T.BINOP(T.OR, T.CONST i, e1)) =
    let val s = munchExp(e1)
    in
      result("T.BINOP(T.OR, T.CONST i, e1)",fn r => emit(A.OPER{assem="ori `d0, `s0, " ^ intToAssemStr(i) ^ "\n",
                  src=[s],
                  dst=[r],
                  jump=NONE}))
    end

    | munchExp (T.BINOP(T.OR, e1, T.CONST i)) =
    let val s = munchExp(e1)
    in
      result("T.BINOP(T.OR, e1, T.CONST i)",fn r => emit(A.OPER{assem="ori `d0, `s0, " ^ intToAssemStr(i) ^ "\n",
                  src=[s],
                  dst=[r],
                  jump=NONE}))
    end

    | munchExp (T.BINOP(T.AND, T.CONST i, e1)) =
    let val s = munchExp(e1)
    in
      result("T.BINOP(T.AND, T.CONST i, e1)",fn r => emit(A.OPER{assem="andi `d0, `s0, " ^ intToAssemStr(i) ^ "\n",
                  src=[s],
                  dst=[r],
                  jump=NONE}))
    end

    | munchExp (T.BINOP(T.AND, e1, T.CONST i)) =
      let val s = munchExp(e1)
      in
        result("T.BINOP(T.AND, e1, T.CONST i)",fn r => emit(A.OPER{assem="andi `d0, `s0, "^intToAssemStr(i) ^ "\n",
                    src=[s],
                    dst=[r],
                    jump=NONE}))
      end

    | munchExp (T.BINOP(oper, e1, e2)) =
      let val s = munchExp(e1)
          val t = munchExp(e2)
          val opstr = binopToString(oper)
      in
        case oper of 
          T.MUL => result("", fn r => (emit(A.OPER{assem="mult `s0, `s1\n", src=[s, t], dst=[], jump=NONE}); 
                                      emit(A.OPER{assem="mflo `d0\n", src=[], dst=[r], jump=NONE})))
        | T.DIV => result("", fn r => (emit(A.OPER{assem="div `s0, `s1\n", src=[s, t], dst=[], jump=NONE}); 
                                                          emit(A.OPER{assem="mflo `d0\n", src=[], dst=[r], jump=NONE})))
        | _ =>  result("T.BINOP(oper, e1, e2)",fn r => emit(A.OPER{assem=opstr ^ " `d0, `s0, `s1\n",
                    src=[s,t],
                    dst=[r],
                    jump=NONE}))
      end

    | munchExp(T.CONST i) =
      result("T.CONST i",fn r=> emit(A.OPER{assem="addi `d0, $zero,"^intToAssemStr(i)^"\n",src=[],dst=[r],jump=NONE}) )

    | munchExp (T.TEMP t) = t

    | munchExp (T.CALL(T.NAME l, expList))  =
      let val munchArgList = munchArgs(0, expList)
      in
          emit(A.OPER{assem="jal " ^ Symbol.name(l) ^ "\n", dst=(Frame.RV::Frame.RA::Frame.callersaves @ Frame.argregs), src=munchArgList, jump=NONE});
          Frame.RV
      end

    | munchExp(T.NAME label) =
      let val labelName = Symbol.name(label)
      in
        result("T.NAME label", fn r => emit(A.OPER{assem="la `d0, " ^ labelName ^ "\n", src = [], dst = [r], jump = NONE}))
      end
    | munchExp(x) =
      let val out = TextIO.stdOut
      in (
        Printtree.printtree(out, T.EXP x);
        Temp.newtemp()
        )
      end

    (*| munchExp (_) = (ErrorMsg.error 0 "Unknown args for munchExp"; Temp.newtemp())*)

  in
   (* munchStm stm;
    !ilist
    *)
      munchStm stm;
      List.rev((!ilist)) (* maximal munch is top down so need to reverse *)
      (*ilist'*)
  end
end
