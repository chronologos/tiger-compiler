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
          
    val show:
      (FlowGraph.iGraph ) -> unit
end
=
struct
    type graphOrdKeyType = int
    structure A = Assem
    structure H = HashTable
    structure fg = IntFuncGraph
    structure tg = TempFuncGraph

  fun stringify2(g, nodeID) =
    let 
      (*val (_) = print("Calling stringify on nodeID " ^ Int.toString(nodeID))*)
      val (_, _, ass, _, _, _) = fg.nodeInfo(fg.getNode(g, nodeID))
    in
      "NodeID " ^ Int.toString(nodeID) ^ " : " ^ ass ^ "\n"
    end

        (* Perform the LiveIns[node] = uses U (LiveOuts[node] - defs) *)
  fun updateLiveIns(liveGraph, nodeID) =
    let 
      (*val (_) = print("Calling stringify on nodeID " ^ Int.toString(nodeID))*)
      val nodeFound = fg.getNode(liveGraph, nodeID)
       val (src, dst, ass, isMove, liveIns, liveOuts) = fg.nodeInfo(nodeFound)
       val updatedLiveIns = FlowGraph.LiveSet.union(src, FlowGraph.LiveSet.difference(liveOuts, dst))
       val updatedGraph = fg.changeNodeData(liveGraph, nodeID, (src, dst, ass, isMove, updatedLiveIns, liveOuts))
    in
      (updatedGraph, (FlowGraph.LiveSet.numItems(updatedLiveIns) <> FlowGraph.LiveSet.numItems(liveIns)))
    end

    (* Perform the LiveOuts[node] = LiveOuts[node] U (LiveIns[succ] for succ in succs[node]) step  *)
  fun addSuccLiveInsToLiveOut(liveGraph, nodeID) =
    let (*val (_) = print("Calling stringify on nodeID " ^ Int.toString(nodeID))*)
        val nodeFound = fg.getNode(liveGraph, nodeID)
      val (src, dst, ass, isMove, liveIns, liveOuts) = fg.nodeInfo(nodeFound)
      val successorsList = fg.succs(nodeFound)
      val result = foldl (fn (x, y) =>
        let 
            (*val (_) = print("Calling stringify on nodeID " ^ Int.toString(x))*)
            val successorNode = fg.getNode(liveGraph, x)
            val (_, _, _, _, succLiveIns, _) = fg.nodeInfo(successorNode)
        in
          FlowGraph.LiveSet.union(succLiveIns, y)
        end) liveOuts successorsList
    in
      (*print("Length of new live-outs for nodeID " ^ Int.toString(nodeID) ^ " : " ^ Int.toString(FlowGraph.LiveSet.numItems(result)));*)
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
    let
      val printStr = FlowGraph.LiveSet.foldl (fn (tmp, str) => str ^ ", " ^ Temp.makestring(tmp) ^ "\n") "" set
    in
      if FlowGraph.LiveSet.numItems(set) = 0
      then "empty \n"
      else printStr
    end
    )

 fun nodeToString (nodeID,nodeData) =
  case nodeData of
      (uses, defs, instr, isMove, liveIns, liveOuts) => (
        ">>NodeID "^ Int.toString(nodeID) ^ " " ^ instr ^ " \n" ^ "live-in: \n" ^ printLiveSets(liveIns) ^ "live-outs: \n" ^ printLiveSets(liveOuts)
      )

  (* Perform 1 iteration of the update with live-ins and live-outs *)
  fun fixedPointLoop(graph) =
    let
        (*val (_) = print("in fixedPointLoop\n")*)
        val (graphRes, changed) = oneIter(graph)
    in
      if changed
      then fixedPointLoop(graphRes)
      else (
        (*fg.printGraph nodeToString stringify graphRes;*)
        (*print("Printing graph without interference");*)
        (*fg.printGraph nodeToString graphRes;*)
        graphRes
      )
    end
    handle fg.NoSuchNode(_) =>(
      print("no such node exception in fixed point loop");
      graph
    )

  fun interfereNode(fgNode, igraph) =
    let
      val (_, defs, _, _, _, liveouts):FlowGraph.nodeDataType = fg.nodeInfo(fgNode)
      val numDef = FlowGraph.LiveSet.numItems(defs)
      fun fold1(def, ig) =
            FlowGraph.LiveSet.foldl (fn(liveout,graph) =>
              let val l = Temp.makestring(liveout)
                  val d = Temp.makestring(def)
              in
                  case String.compare(l,d) of
                    EQUAL => graph
                  | _ => tg.doubleEdge(graph,l,d)
              end
            ) ig liveouts
    in
      case Int.compare(numDef, 1) of 
        GREATER => (
          (* NOT AN ERROR. could be trashed regs from function call *)
          FlowGraph.LiveSet.foldl fold1 igraph defs
        )
      | LESS => igraph
      | EQUAL => (
        (* only here if numitems is 1 *)
        let
          val def = hd (FlowGraph.LiveSet.listItems(defs))
        in
          FlowGraph.LiveSet.foldl (fn(liveout,igraph) =>
          let
            val l = Temp.makestring(liveout)
            val d = Temp.makestring(def)
          in
            case String.compare(l,d) of 
            EQUAL => igraph
            | _ => (
            (* print("getting edge info\n"); *)
            tg.doubleEdge(igraph, l, d)
            )
          end
          ) igraph liveouts
        end
      )
    end

  fun makeIGraphNodes(graph:FlowGraph.graph) =
    let
      val nodes = fg.nodes(graph)
      val temps =
        foldl (fn(x,s)=>
          let
            val (defs,uses,_,_,li,lo):FlowGraph.nodeDataType = fg.nodeInfo(x)
            val a = FlowGraph.LiveSet.union(s, defs)
            val a' = FlowGraph.LiveSet.union(a, uses)
            val a'' = FlowGraph.LiveSet.union(a', li)
            val a''' = FlowGraph.LiveSet.union(a'', lo)
          in
            a'''
          end
          ) FlowGraph.LiveSet.empty nodes
      val tempsList = FlowGraph.LiveSet.listItems(temps)
    in
      foldl (fn(x,g) =>
        tg.addNode(g,Temp.makestring(x),x)
      ) tg.empty tempsList
    end

  fun nodeToStringTemp (nodeID,nodeData) =
    case nodeData of
      tmp => (
        "NodeID "^ Temp.makestring(tmp) ^"\n"
      )

  fun stringify2Temp(g, nodeID) = (
    (* print("Getting node info for ID " ^ nodeID); *)
    let val tmp = tg.nodeInfo(tg.getNode(g, nodeID))
    in
      "NodeID " ^ nodeID ^ " : " ^ Temp.makestring(tmp) ^ "\n"
    end
    )
  fun interferenceGraph(flowGraph:FlowGraph.graph) =
    let
      val tempGraph = tg.empty
      fun buildMoveList(g:FlowGraph.graph): (Temp.temp * Temp.temp) list =
        let val flowGraphList = fg.nodes(g)
        in
        foldl (fn (x,res) =>
                  let val (uses,defs,_,isMove,_,_):FlowGraph.nodeDataType = fg.nodeInfo(x)
                  in
                    if (isMove andalso FlowGraph.LiveSet.numItems(uses) = 1 andalso FlowGraph.LiveSet.numItems(defs) = 1) then (hd (FlowGraph.LiveSet.listItems(uses)), hd (FlowGraph.LiveSet.listItems(defs)))::res else res
                  end
              ) [] flowGraphList
        end
      val moveList = buildMoveList(flowGraph)
      val iGraph: FlowGraph.iGraph = makeIGraphNodes(flowGraph)
      val iGraph':FlowGraph.iGraph = foldl interfereNode iGraph (fg.nodes(flowGraph))
    in
      (*print("************** Printing interference graph ***************");
      tg.printGraph2 nodeToStringTemp stringify2Temp iGraph'; *)
      (iGraph', moveList)
    end
    
  fun show(g) = 
    let
        val nodes = tg.nodes(g)
        fun println x = print(x ^"\n")
        fun prOneNode(g,temp) = 
            let val adjs = TempFuncGraph.adj' g temp 
                val temps = map(tg.nodeInfo) adjs
                val strs = map (Temp.makestring) temps
                val res = foldr (fn(x,res) => x ^ " " ^ res) "" strs
            in
                Temp.makestring(tg.nodeInfo(temp)) ^ "->" ^ res
            end
    in
        app (fn(x)=>println(prOneNode(g,x))) nodes
    end
     
end
