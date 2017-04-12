structure MakeGraph:
(*'a = (uses:Temp.temp list * defs:Temp.temp list * instruction/label * isMove:bool)*)
sig 
    (*val instrs2graph: Assem.instr list -> 'a IntFuncGraph.graph*)
    (*val instrs2graph: Assem.instr list -> flowgraph*)
    val instrs2graph:  Assem.instr list -> FlowGraph.graph
    val stringify: int * ('a * 'b * string * 'c * 'd * 'e) -> string 
    val getMaxNodeID: unit -> int

end

= struct 
    type graphOrdKeyType = int
    structure A = Assem
    structure H = HashTable
    exception MakeGraph
    structure fg = IntFuncGraph
    structure LiveSet = FlowGraph.LiveSet 
    
    (*structure LiveSet = SplaySetFn(type ord_key = Temp.temp fun compare(x,y)=String.compare(Temp.makestring x, Temp.makestring y))
    type nodeDataType = (LiveSet.set * LiveSet.set * string * bool * LiveSet.set * LiveSet.set)
    type flowgraph =  nodeDataType fg.graph*)
    
    
    (*val labelNodesMap = Symbol.enter(Symbol.empty,Symbol.symbol("")) (* label -> Node map *)*)
    val sizeHint = 128
    val labelNodesMap : (string,int) H.hash_table =
		H.mkTable(HashString.hashString, op = ) (sizeHint,MakeGraph)
		  
    val nextNodeID =  ref 0
    val maxNodeID = ref 0
    
    fun resetNodeID () = 
        (maxNodeID := !maxNodeID - 1;
        nextNodeID := 0)
    
    fun getNextNodeID () =
        let
            val nodeID = !nextNodeID
        in
            (nextNodeID := nodeID + 1;
            if nodeID > !maxNodeID then maxNodeID := nodeID else ();
            (*print("nextNodeID:" ^ Int.toString(nodeID) ^ "\n");
            print("Max nodeID: " ^ Int.toString(!maxNodeID) ^ "\n");*)
            nodeID)
        end
        
    fun getMaxNodeID () = !maxNodeID
    
    
    fun foldAssemCreateNodesFn (nextInst, graphSoFar) =
        (* nodes for labels *)
        case nextInst of 
        A.LABEL({assem=ass, lab=lab}) =>
            let val newNodeID = getNextNodeID()
                val (newGraph, newLabelNode) = (fg.addNode'(graphSoFar, newNodeID, (LiveSet.empty, LiveSet.empty, ass, false, LiveSet.empty, LiveSet.empty)))
            in
                (
                (*labelNodesMap := Symbol.enter(!labelNodesMap, lab, newNodeID);*)
                 H.insert labelNodesMap (Symbol.name(lab),newNodeID);
                newGraph)
            end
            
        | A.OPER({assem = ass, dst = dst, src = src, jump=_}) => 
            let 
                val (newGraph, _) = (fg.addNode'(graphSoFar, getNextNodeID(), (LiveSet.addList(LiveSet.empty, src), LiveSet.addList(LiveSet.empty, dst), ass, false, LiveSet.empty, LiveSet.empty)))
            in  
                newGraph
            end
        | A.MOVE({assem = ass, dst = dst, src = src}) =>
            let val src = [src]
                val dst = [dst]
                val (newGraph, _) = (fg.addNode'(graphSoFar, getNextNodeID(), (LiveSet.addList(LiveSet.empty, src), LiveSet.addList(LiveSet.empty, dst), ass, true, LiveSet.empty, LiveSet.empty)))
            in  
                newGraph
            end
     (*   
        (* get a particular node, raises NoSuchNode if not found*)
    val getNode: 'a graph * nodeID -> 'a node
    val changeNodeData: 'a graph * nodeID * 'a -> 'a graph   
     *)
     
    fun addNextInsEdge(myID,graphSoFar) = 
        let val nextLineNode = if myID = !maxNodeID
                               then NONE 
                               else SOME(fg.getNode(graphSoFar, myID + 1))
            val graphWithNextLine =
                if isSome nextLineNode 
                then 
                    fg.addEdge(graphSoFar, {from=myID, to=(myID + 1)})
                else
                    graphSoFar
        in
            graphWithNextLine
        end 
    
    (* TODO: reset nextNodeID ref to 0 to get the same nodeIDs as before *)                        
    fun foldAssemLinkFn (nextInst, graphSoFar) =
        (* For each node, if jump is empty list, then add to successors set graphSoFar*)
        let val myID = getNextNodeID()

            (* val info = fg.nodeInfo(node) *)
        in
            case nextInst of 
                A.OPER({assem = ass, dst = dst, src = src, jump = jmp}) =>
                    (case jmp of 
                        NONE => addNextInsEdge(myID,graphSoFar) 
                      | SOME(jList) => 
                            let 
                                (*val (_) = print("My ID is " ^ Int.toString(myID) ^ "\n")*)
                                val graphWithNextLine = addNextInsEdge(myID,graphSoFar) 
                                val jumpNodeIDOpt = H.find labelNodesMap (Symbol.name(List.hd(jList)))
                                val len = List.length(jList)
                            in
                                if len = 0
                                then graphSoFar
                                else(
                                    case jumpNodeIDOpt of 
                                        NONE => (graphWithNextLine)
                                      | SOME(x) => 
                                            fg.addEdge(graphWithNextLine, {from = myID, to=x})
                                )
                            end
                    )
                 
               | (_) => addNextInsEdge(myID,graphSoFar)
        end
        handle fg.NoSuchNode(nid) =>(
          print("no such node exception in foldAssemLinkFn "^ Int.toString(nid) ^ "\n");
          graphSoFar
        )

    
    fun stringify(nodeID, data) : string =
        (* Check data type of data, whether 4-tup or 6-tup *)
        case data of (_, _, ass, _, _, _) =>
            "Node#" ^ Int.toString(nodeID) ^ " : " ^ ass
        (* Exhaustive? *)
    
    
    fun instrs2graph(instrs:Assem.instr list) = 
        (*
            - it's a foldl
            - 1st foldl: create nodes for all instrs, create nodes for labels too; create map(label -> nodes)
            - 2nd foldl: link all nodes based on code order and jumps (jump field in Assem.Oper); 
                         update uses and defs (src, dst fields in Assem.Oper);
          
        *)
        let val completeGraph = foldl foldAssemCreateNodesFn fg.empty instrs 
            val dumdum = resetNodeID()
            val completeLinkGraph = foldl foldAssemLinkFn completeGraph instrs 
        in  
            (*print("instrs2graph result: \n");*)
            (*(print(" ================ COMPLETE GRAPH ==================  ");*)
            (*fg.printGraph stringify completeGraph;*)
            (nextNodeID := 0;
            maxNodeID := 0;
            completeLinkGraph)
        end
            
end