signature COLOR =
sig
  structure Frame : FRAME
  
  type allocation = Frame.register Temp.Table.table

  val color: {interference: FlowGraph.igraph,
              moves: (Temp.temp * Temp.temp) list,
              initial: allocation,
              spillCost: Graph.node -> int,
              registers: Frame.register list}
              -> allocation * Temp.temp list
  end
