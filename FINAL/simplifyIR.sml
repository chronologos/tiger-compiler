signature SIMPLIFYIR =
sig
    type simFrag
    val simplify : MipsFrame.frag list -> simFrag list
end

structure SimplifyIR : SIMPLIFYIR =
struct
  structure Frame = MipsFrame
  structure C = Canon
  datatype simFrag =  STRING of Frame.STRING
                    | PROC of {body:Tree.stm list, frame: Frame.frame}

  fun simplify (fragList:Frame.frag list) =
      let fun foldFn (frag,resList) =
          case frag of
              Frame.STRING (_) => frag::resList
            | Frame.PROC({body=bodyStm,frame=frame}) =>
                  let val linearized = C.linearize(bodyStm)
                      val basicBlocks = C.basicBlocks(linearized)
                      val traceSchedule = C.traceSchedule(basicBlocks)
                  in
                      simFrag.PROC({body=traceSchedule,frame=frame})
                  end
      in
          foldr foldFn [] fragList
      end
end
