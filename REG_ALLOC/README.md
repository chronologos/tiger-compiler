# Register Allocation
- Global Stack
- Build stage: Liveness2.sml generates FlowGraph.iGraph.
- regalloc.sml calls colors.sml as subroutine.
    -   val alloc : Assem.instr list * Frame.frame
                  -> Assem.instr list * allocation
- colors.sml:
- datastructures:
    - precolored: node list 
    - initial: node list 
    - simplifyWorklist: node list
    - freezeWorklist: node list
    - spillWorklist: node list
    - spilledNodes: node list
    - coalescedNodes: node list
    - coloredNodes: node list (? how to store color), should this be Frame.register Temp.Table.table?
    - selectStack: node list
    - coalescedMoves, frozenMoves, worklistMoves, activeMoves: (Temp.temp * Temp.temp) list
- fns:
    - fun addPrecolored(...): Frame.register Temp.Table.table -> Frame.register Temp.Table.table
    -                          add precolored nodes to register allocation table.
    - fun simplify(...): FlowGraph.iGraph -> FlowGraph.iGraph
    - fun spill(...): FlowGraph.iGraph -> (FlowGraph.iGraph * ?)
    -   if we want to go for interprocedural liveness extra credit, might be better to spill in IR. Inverse venv: temp -> var needed.
    -   if we dont', easier to do in assembly. we know temps that spill, for every appearance in uses we replace it with a mem load? But we will need the current frame layout. this can be obtained in Main2 fun emitproc out (F.PROC({body,frame})).
    - fun coalesce(...): FlowGraph.iGraph * (Temp.temp * Temp.temp) list -> FlowGraph.iGraph
    -                   implement either/both or briggs/george
    - fun unfreeze(...): FlowGraph.iGraph * (Temp.temp * Temp.temp) list -> (Temp.temp * Temp.temp) list 
    -                  look for a move-related node of low degree, unfreeze the node, meaning that it is no longer constrained. In effect, we need to remove all moves involving this node from the list.
    - fun select(...):  FlowGraph.iGraph.node list -> FlowGraph.iGraph -> (SOME([Frame.register Temp.Table.table]) * FlowGraph.iGraph.node list)
    -                     stack of temps              empty igraph (at start)      (allocation result * spill nodes) 
    -                     optional: coalesce spilled nodes - may not be very useful on ISA with many registers.

# Run Chapter 11
Main.compile "dir/to/file"

