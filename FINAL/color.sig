signature COLOR =
sig
  structure Frame : FRAME
  
  type allocation = Temp.temp Temp.Map.map

  val color: FlowGraph.iGraph *
              allocation
              -> allocation * Temp.temp list
  end
signature COLOR =
sig
  structure Frame : FRAME
  
  type allocation = Temp.temp Temp.Map.map

  val color: FlowGraph.iGraph *
              allocation
              -> allocation * Temp.temp list
  end
