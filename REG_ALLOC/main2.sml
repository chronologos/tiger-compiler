structure Main = struct

  structure Tr = Translate
  structure F = MipsFrame
  (*structure R = RegAlloc*)

  val first = ref true 
  val strings:(Symbol.symbol * string) list ref = ref []
  fun emitproc out (F.PROC({body,frame})) =
    let
      (*val _ = print("Max offset is " ^ Int.toString(F.getMaxOffset(frame)))*)
      val {prolog=prolog, epilog=epilog}=F.procEntryExit3(frame)
      val {params=pa, loads=l, saves=s}=F.procEntryExit1(frame)
      val saveArgs = F.procEntryExit4(frame)
      val _ = print ("emit " ^ Symbol.name (F.name frame) ^ "\n")
      val stms = Canon.linearize body
      val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
      (* val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
      (*val _ = app (fn x => Printtree.printtree(out, x)) stms'*)
      val instrs = List.concat(map (MipsGen.codegen frame) stms')
      val instrs' = List.take(instrs, 1) @ saveArgs @ List.drop(instrs, 1)
      val (instrs'',alloc) = Regalloc.alloc(instrs',frame)
      val saytemp = Regalloc.makeSayTemp alloc
      (* qtspim initializes FP to 0, need to set it to SP for outermost frame *)
      val instrs''' = (if (!first)
        then (first := false;List.take(instrs'', 1) @ [Assem.MOVE{assem="move $fp, $sp\n", dst=F.FP, src=F.SP}] @  prolog @ pa @ s @ List.drop(instrs'', 1) @ l @ epilog)
        else (List.take(instrs'', 1) @  prolog @ pa @ s @ List.drop(instrs'', 1) @ l @ epilog) )
      val format0 = Assem.makeformat(Temp.makestring)
      val format1 = Assem.makeformat(saytemp)
      
    in (
      app (fn i => TextIO.output(out,format0 i)) instrs
      (*app (fn i => TextIO.output(out,format1 i)) instrs'''*)
     ) 
    end
  | emitproc out (F.STRING(lab,s)) = strings:=((lab,s) :: (!strings)) (*TextIO.output(out,F.string(lab,s))*)


  fun withOpenFile fname f =
    let val out = TextIO.openOut fname
    in (f out before TextIO.closeOut out)
	  handle e => (TextIO.closeOut out; raise e)
    end

  fun compile filename =
    let val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
        
    in 
      withOpenFile (filename ^ ".s")
      (fn out => (
            app (emitproc out) frags; 
            TextIO.output(out, ".data\n"); 
            app (fn (y,x) => 
            (TextIO.output(out, Symbol.name(y)^": .word "^ Int.toString(String.size(x)) ^ "\n.ascii " ^ "\"" ^ x ^ "\"\n")
            )) (!strings)
            )
      )
      
    end
end
