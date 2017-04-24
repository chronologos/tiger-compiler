structure Main = struct

  structure Tr = Translate
  structure F = MipsFrame
  (*structure R = RegAlloc*)


  fun emitproc out (F.PROC({body,frame})) =
    let
      val _ = print ("emit " ^ Symbol.name (F.name frame) ^ "\n")
      val stms = Canon.linearize body
      (* val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
      val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
      val _ = app (fn x => Printtree.printtree(out, x)) stms'
      val instrs0 = List.concat(map (MipsGen.codegen frame) stms')
      val format0 = Assem.makeformat(Temp.makestring)
      val (instrs,alloc) = Regalloc.alloc(instrs0,frame)
      val saytemp = Regalloc.makeSayTemp alloc
      val format1 = Assem.makeformat(saytemp)
      
    in
      app (fn i => TextIO.output(out,format0 i)) instrs0;
      app (fn i => TextIO.output(out,format1 i)) instrs
      
    end
  | emitproc out (F.STRING(lab,s)) = TextIO.output(out,F.string(lab,s))


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
      (fn out => (app (emitproc out) frags))
    end
end
