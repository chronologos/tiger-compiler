structure Regalloc : REG_ALLOC =
struct

    type allocation = Temp.temp Temp.Map.map
    structure TG = TempFuncGraph
    structure Frame = MipsFrame
    val K = Temp.Set.numItems(Frame.usableRegisters) (* # of available colors *)

    fun alloc(instrs:Assem.instr list, frame:Frame.frame) =
        let val flowGraph = MakeGraph.instrs2graph(instrs)
            val updatedGraph = Liveness.fixedPointLoop(flowGraph) 
            val (iG, moveList) = Liveness.interferenceGraph(updatedGraph)
            (*val _ = print("IGRAPH\n")
            val _ = Liveness.show(iG,moveList) *)
            
            val _ = print("Printing ALLOCATION")
        
            val (regAlloc, spillList) =  Color.color(iG, Frame.regTable)
        in
            (instrs, regAlloc)
        end
                        
        
   (* val color: {interference: FlowGraph.iGraph,
              initial: allocation,
              registers: Temp.temp list}
              -> allocation * Temp.temp list *)     
   
    fun makeSayTemp alloc = let
        fun chooseColor(x) =
          let val chosenColorOpt = Temp.Map.find(alloc, x)
              val chosenColor = if isSome chosenColorOpt 
                                then valOf chosenColorOpt 
                                else (ErrorMsg.error 0 ("GG missing allocation for " ^ Temp.makestring(x));Frame.ERROR)
          in
              Temp.getNameFromNamedTemp(Temp.makestring chosenColor)
          end 
        in
          chooseColor
        end
end



(*signature COLOR =
sig
  structure Frame : FRAME
  
  type allocation = Frame.register Temp.Table.table

  val color: {interference: FlowGraph.igraph,
              moves: (Temp.temp * Temp.temp) list,
              initial: allocation,
              spillCost: Graph.node -> int,
              registers: Frame.register list}
              -> allocation * Temp.temp list
  end*)
