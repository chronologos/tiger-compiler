structure Liveness:
  (*
    liveness:
    - while loop: update live-in & live-out of all nodes until see a fixed point
    - after while: add all interference edges (between live-outs and (new-def & live-outs) )
  *)
sig
    val fixedPointLoop: 
          FlowGraph.graph -> FlowGraph.graph
          
    val interferenceGraph:  
          FlowGraph.graph -> (FlowGraph.iGraph * (Temp.temp * Temp.temp) list)
end
= 
struct
    type graphOrdKeyType = int
    structure A = Assem
    structure H = HashTable
    structure fg = IntFuncGraph
    structure tg = TempFuncGraph

  fun makeIGraphNodes(graph:FlowGraph.graph) =
  let
    val nodes = fg.nodes(graph)
    val temps =
      foldl (fn(x,s)=>
        let
          val (_,_,_,_,li,lo):FlowGraph.nodeDataType = fg.nodeInfo(x)
        in
          FlowGraph.LiveSet.union((FlowGraph.LiveSet.union(s,lo)),li)
        end
        ) FlowGraph.LiveSet.empty nodes
    val tempsList = FlowGraph.LiveSet.listItems(temps)
  in
    foldl (fn(x,g) =>
      tg.addNode(g,Temp.makestring(x),x)
    ) tg.empty tempsList
  end
  
  fun stringify2(g, nodeID) = 
    let val (_, _, ass, _, _, _) = fg.nodeInfo(fg.getNode(g, nodeID))
    in
      "NodeID " ^ Int.toString(nodeID) ^ " : " ^ ass ^ "\n"
    end
  
  fun stringify2Temp(g, nodeID) = 
    let val tmp = TempFuncGraph.nodeInfo(TempFuncGraph.getNode(g, nodeID))
    in
      "NodeID " ^ nodeID ^ " : " ^ Temp.makestring(tmp) ^ "\n"
    end
   
        (* Perform the LiveIns[node] = uses U (LiveOuts[node] - defs) *)     
  fun updateLiveIns(liveGraph, nodeID) =
    let val nodeFound = fg.getNode(liveGraph, nodeID)
       val (src, dst, ass, isMove, liveIns, liveOuts) = fg.nodeInfo(nodeFound)
       val updatedLiveIns = FlowGraph.LiveSet.union(src, FlowGraph.LiveSet.difference(liveOuts, dst))
       val updatedGraph = fg.changeNodeData(liveGraph, nodeID, (src, dst, ass, isMove, updatedLiveIns, liveOuts))
      (* val _ =print("Length of old live-ins for nodeID " ^ Int.toString(nodeID) ^ " : " ^ Int.toString(FlowGraph.LiveSet.numItems(liveIns))) *)

    in
    (*  print("Length of new live-ins for nodeID " ^ Int.toString(nodeID) ^ " : " ^ Int.toString(FlowGraph.LiveSet.numItems(updatedLiveIns))); *)
      (updatedGraph, (FlowGraph.LiveSet.numItems(updatedLiveIns) <> FlowGraph.LiveSet.numItems(liveIns)))
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
          FlowGraph.LiveSet.union(succLiveIns, y)
        end) liveOuts successorsList
    in
      print("Length of new live-outs for nodeID " ^ Int.toString(nodeID) ^ " : " ^ Int.toString(FlowGraph.LiveSet.numItems(result)));
      (fg.changeNodeData(liveGraph, nodeID, (src, dst, ass, isMove, liveIns, result)), (FlowGraph.LiveSet.numItems(liveOuts) <> FlowGraph.LiveSet.numItems(result)))
    end
    
  fun update(graph, nodeID) =  
    let
      val (graph', liveOutChanged) = addSuccLiveInsToLiveOut(graph, nodeID)
      val (graph'', liveInChanged) = updateLiveIns(graph', nodeID)
    in
    
      (graph'', liveInChanged orelse liveOutChanged)
    end
   
fun oneIter(graph) =
    let val nodes = fg.nodes(graph)
        val nodeIDList = map (fg.getNodeID) nodes 
        fun iterFoldFn(nextID, (graphSoFar, changed)) = 
          let val (newGraph, changedNow) = update(graphSoFar, nextID)
          in
            (newGraph, changed orelse changedNow)
          end
          
    in
      foldl iterFoldFn (graph, false) nodeIDList
    end
    
fun printLiveSets(set) = (
    let val printStr = FlowGraph.LiveSet.foldl (fn (tmp, str) => str ^ ", " ^ Temp.makestring(tmp) ^ "\n") "" set
    in
      if FlowGraph.LiveSet.numItems(set) = 0 
      then "empty \n"
      else printStr
    end
  )

 fun nodeToString (nodeID,nodeData) =
  case nodeData of 
      (uses, defs, instr, isMove, liveIns, liveOuts) => (
        "NodeID "^ Int.toString(nodeID) ^ " " ^ instr ^ " \n" ^ "live-in: \n" ^ printLiveSets(liveIns) ^ "live-outs: \n" ^ printLiveSets(liveOuts)
      )
  
  fun nodeToStringTemp (nodeID,nodeData) =
  case nodeData of 
      tmp => (
        "NodeID "^ Temp.makestring(tmp) ^"\n"
      )
    
  (* Perform 1 iteration of the update with live-ins and live-outs *)
  fun fixedPointLoop(graph) =
  
    let 
        val (_) = print("in fixedPointLoop\n")
        val (graphRes, changed) = oneIter(graph)
    in
       if changed 
       then fixedPointLoop(graphRes) 
       else (
         (*fg.printGraph nodeToString stringify graphRes;*)
         print("Printing graph without interference");
         fg.printGraph2 nodeToString stringify2 graphRes;
         graphRes
       )
    end    

  (*
  fun dfs([], (graph, changed)) = (graph, changed)
    | dfs(predList, (graph, changed)) = 
      let
        val (_) = print("dfs\n");
        fun foldPredFun (hd, (graphSoFar,changed)) =
          let val hdPreds = fg.preds(fg.getNode(graphSoFar,hd)) 
              val (updatedGraph, changedNow) = update(graphSoFar, hd)
              val (updatedGraph', changedRes) = dfs(hdPreds, (updatedGraph, changed orelse changedNow))
          in
            (updatedGraph',changedRes)
          end
      in
        foldl foldPredFun (graph, changed) predList
      end
    
  *)    
  
  
  fun findNodeOrCreate(temp,tGraph) =
    let val tempStr = Temp.makestring temp
        val foundNode = TempFuncGraph.getNode(tGraph, tempStr) 
    in
    (tGraph, foundNode)
    end
    
    handle TempFuncGraph.NoSuchNode(nid) => (
      print("Node " ^ Temp.makestring(temp) ^ " does not exist yet\n");
      let val newGraph = TempFuncGraph.addNode(tGraph, Temp.makestring temp, temp)
          val newNode = TempFuncGraph.getNode(newGraph, Temp.makestring temp)
      in
          (newGraph,newNode)
      end
    )
      
    


(*    val addNode: 'a graph * nodeID * 'a -> 'a graph *)

(* val doubleEdge : 'a graph * nodeID * nodeID -> 'a graph   *)
    
    fun getIDFromTemp(G, t) = 
      let val (g,n) = findNodeOrCreate(t,G) 
      in  TempFuncGraph.getNodeID(n)
      end

    fun sameTempNode(n1, n2) = (* LESS | EQUAL |  *)
      case String.compare(Temp.makestring(n1),Temp.makestring(n2))  of 
          EQUAL => true
         | (_) => false

    fun interferenceGraph (flowGraph) =
      let val tempGraph = makeIGraphNodes(flowGraph)
          fun buildMoveList(g) =
            let val flowGraphList = IntFuncGraph.nodes(g)
            in
            foldl (fn (x,res) => 
                      let val (uses,defs,_,isMove,_,_) = IntFuncGraph.nodeInfo(x)  
                      in
                        if (isMove andalso FlowGraph.LiveSet.numItems(uses) = 1 andalso FlowGraph.LiveSet.numItems(defs) = 1) 
                        then (hd(FlowGraph.LiveSet.listItems(uses)), hd(FlowGraph.LiveSet.listItems(defs)))::res
                        else res
                      end
                  ) [] flowGraphList
            end
          val moveList = buildMoveList(flowGraph)
          
          fun interfereNode(fgNode, tempGraph) =
            let val (_, defs, _, _, _, liveOuts) = fg.nodeInfo(fgNode)
                (* make double-edge between this node and each graph node in defs::liveOuts *)
                val nodeList = FlowGraph.LiveSet.listItems(FlowGraph.LiveSet.union(defs, liveOuts))
                
                fun innerFoldFn(innerNode, (nodeList, tempGraphSoFar)) =
                
                  let val (tGraph,tempNode) = findNodeOrCreate(innerNode, tempGraphSoFar)
                      
                      fun addDoubleEdge(tempNode,(nextLiveOut,tGraph)) =
                        if not (sameTempNode(tempNode, nextLiveOut)) then
                          (nextLiveOut,TempFuncGraph.doubleEdge(tGraph, getIDFromTemp(tGraph, nextLiveOut), Temp.makestring(tempNode)))
                        else (nextLiveOut,tGraph)
                      val (node',graphRes) = foldl addDoubleEdge (innerNode,tempGraphSoFar) nodeList;

                  in
                    (nodeList,graphRes)
                  end
                    
            in
              let val (doneWithNode, graphWithNode) = foldl innerFoldFn (nodeList, tempGraph) nodeList
              in
                graphWithNode
              end
            end  
            
            (* ALTERNATIVE:
            fun interfereNode(fgNode, graph) = 
            let val (_, defs, _, _, _, liveOuts) = fg.nodeInfo(fgNode)
                val lenDef = FlowGraph.LiveSet.numItems(defs)
                fun addDoubleEdge (n1,n2,graph) = 
                fun foldLiveOutsFn (liveOutTemp, (defTemp,graph)) =
                    (defTemp,addDoubleEdge(liveOutTemp,defTemp,graph))
            in
                if lenDef = 0 
                then tempGraph
                else (
                  let (_, doneGraph) = FlowGraph.LiveSet.foldl foldLiveOutsFn (List.hd(FlowGraph.LiveSet.listItems(defs)), tempGraph) liveOuts
                  in
                    doneGraph
                  end
                )
            end
            
            *)
            val graph = foldl interfereNode tempGraph (IntFuncGraph.nodes(flowGraph))
      in
          print("Printing interference graph");
          TempFuncGraph.printGraph2 nodeToStringTemp stringify2Temp graph;
          (graph, moveList)
      end    
        
        
        
   
end
