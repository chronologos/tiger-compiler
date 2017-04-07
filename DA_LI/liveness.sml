structure Liveness:
  (*
    liveness:
    - while loop: update live-in & live-out of all nodes until see a fixed point
    - after while: add all interference edges (between live-outs and (new-def & live-outs) )
  *)
sig
    val interferenceGraph: 
          MakeGraph.flowgraph -> MakeGraph.flowgraph 
end
= 
struct
(*

liveness.sml:14.41-14.56 Error: unbound type constructor: graphOrdKeyType
liveness.sml:48.9-48.95 Error: operator and operand don't agree [circularity]
  operator domain: (LiveSet.set * LiveSet.set * 'Z * 'Y * 'X * LiveSet.set) 
                     fg.graph * fg.nodeID * 
                   (LiveSet.set * LiveSet.set * 'Z * 'Y * 'X * LiveSet.set)
  operand:         (LiveSet.set * LiveSet.set * 'Z * 'Y * 'X * LiveSet.set) 
                     fg.graph * fg.nodeID * 
                   (LiveSet.set * LiveSet.set * 'Z * 'Y * 
                    ((LiveSet.set * LiveSet.set * 'Z * 'Y * 'X * LiveSet.set) 
                       fg.graph * fg.nodeID
                     -> 'W) * LiveSet.set)
  in expression:
    fg.changeNodeData
      (liveGraph,nodeID,(src,dst,ass,isMove,updateLiveIns,liveOuts))
val it = false : bool

*)

    type graphOrdKeyType = int
    structure A = Assem
    structure H = HashTable
    structure fg = FuncGraph(type ord_key=graphOrdKeyType
                                    fun compare(x,y) = 
                                        if x<y then LESS
                                        else(
                                            if x>y then GREATER
                                            else EQUAL
                                            )
                                    )
    
    structure LiveSet = SplaySetFn(type ord_key = Temp.temp fun compare(x,y)=String.compare(Temp.makestring x, Temp.makestring y))
    type nodeDataType = (LiveSet.set * LiveSet.set * string * bool * LiveSet.set * LiveSet.set)
    type flowgraph =  nodeDataType fg.graph
  
               
    (* val getNode: 'a graph * nodeID -> 'a node 
    
      val nodeInfo: 'a node -> 'a
    
      (src, dst, ass, false, LiveSet.empty, LiveSet.empty)
    val union : (set * set) -> set 

    val succs:  'a node -> nodeID list

    val addList : (set * item list) -> set 
  val difference : (set * set) -> set 

    val changeNodeData: 'a graph * nodeID * 'a -> 'a graph

    *)   
    
    (* Perform the LiveIns[node] = uses U (LiveOuts[node] - defs) *)     
          
    fun updateLiveIns(liveGraph, nodeID) =
       let val nodeFound = fg.getNode(liveGraph, nodeID)
          val (src, dst, ass, isMove, liveIns, liveOuts) = fg.nodeInfo(nodeFound)
          val updatedLiveIns = LiveSet.union(src, LiveSet.difference(liveOuts, dst))
      in
        fg.changeNodeData(liveGraph, nodeID, (src, dst, ass, isMove, updatedLiveIns, liveOuts))
      end
               
    (* Perform the LiveOuts[node] = LiveOuts[node] U (LiveIns[succ] for succ in succs[node]) step  *)           
               
    fun addSuccLiveInsToLiveOut(liveGraph, nodeID) =
      let val nodeFound = fg.getNode(liveGraph, nodeID)
          val (src, dst, ass, isMove, liveIns, liveOuts) = fg.nodeInfo(nodeFound)
          val successorsList = fg.succs(nodeFound)
          val result = foldl (fn (x, y) => 
            let val successorNode = fg.getNode(liveGraph, x)
                val (_, _, _, _, succLiveIns, _) = fg.nodeInfo(successorNode)
            in
              LiveSet.union(succLiveIns, y)
            end) liveOuts successorsList
      in
        fg.changeNodeData(liveGraph, nodeID, (src, dst, ass, isMove, liveIns, result))
      end
    
    fun interferenceGraph (iG) = iG
        
end