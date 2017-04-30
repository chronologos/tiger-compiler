structure Color :> COLOR 
= struct
 
    type allocation = Temp.temp Temp.Map.map
    
    structure StringSet = SplaySetFn(type ord_key=string
                                     fun compare(x,y) = String.compare(x,y))
    structure TG = TempFuncGraph
    structure Frame = MipsFrame

    fun tempSet2StrSet(tempSet) = 
        let val itemsList = Temp.Set.listItems(tempSet)
            val strList = map Temp.makestring itemsList 
        in
            StringSet.addList(StringSet.empty, strList)
        end


    fun simplify(iG:FlowGraph.iGraph, stack: Temp.temp TG.node list) =
    let 
        val simplifyWorklist = List.filter (fn x => (
                                TG.inDegree(x) < Frame.NUMREG
                                andalso 
                                (not (isSome(Temp.Set.find (fn y => y = TG.nodeInfo(x)) (Frame.allRegisters)))) 
                )) (TG.nodes(iG))
        val done = List.length(simplifyWorklist) = 0
        fun foldFn (nextNode, (graphSoFar, stackSoFar)) = (TG.removeNode(graphSoFar, TG.getNodeID(nextNode)), nextNode::stackSoFar)
    in
        if done then
            (iG, stack)
        else 
            let val (newGraph, newStack) = foldl foldFn (iG, stack) simplifyWorklist
            in
                simplify(newGraph, newStack)
            end
    end    

    fun SelectPotentialSpill(graph, stack:Temp.temp TG.node list, igraph) =
    let
        (* Check graph for non pre-colored nodes with high degree, add them to a list *)
        val spillWorklist = List.filter (fn x => (
                                TG.inDegree(x) >= Frame.NUMREG 
                                andalso 
                                (not (isSome(Temp.Set.find (fn y => (y = TG.nodeInfo(x))) Frame.allRegisters)))
                                )) (TG.nodes(graph))
        val needToSpill = List.length(spillWorklist) <> 0
    in
        if not needToSpill then (
            (graph, stack, false)
        ) else
            let val newStackHead = hd spillWorklist
            in (
                print("SPILL:\n");
                app (fn x =>print(Temp.makestring(TG.nodeInfo(x))^",")) spillWorklist;
                print("\n");
                Liveness.show(graph);
                (TG.removeNode(graph, TG.getNodeID(newStackHead)), newStackHead::stack, true)
                )
            end
    end
    
    fun colorLoop(iG:FlowGraph.iGraph, stack:Temp.temp TG.node list) =
        let
            val (iG',stack) = simplify(iG,[])
            val (graphWithSpill, stackWithSpill, spilled) = SelectPotentialSpill(iG', stack, iG) 
        in
            if spilled then colorLoop(graphWithSpill, stackWithSpill) 
            else (graphWithSpill, stackWithSpill)
        end
    
    
           
    (* one color a node can possibly be assigned to *)
    fun colorOneNode(iG, reducediG, assignment:Temp.temp Temp.Map.map, node:Temp.temp TG.node) =
    let
        val originalAdj = StringSet.addList(StringSet.empty,TG.adj(node)) (* set of adj nodeID in iG *)
        val presentNodeList = map (TG.getNodeID) (TG.nodes(reducediG))
        val presentAdj = StringSet.addList(StringSet.empty, presentNodeList) 
        val intersectAdj = StringSet.intersection(presentAdj,originalAdj)
        
        val colorSet = StringSet.map (fn (n) => case Temp.Map.find(assignment,TG.nodeInfo(TG.getNode(iG, n))) of
                                                    SOME(temp) => Temp.makestring(temp)
                                                    | NONE => (ErrorMsg.error 0 ("SPILL WARNING\n"); Temp.makestring(Frame.ERROR))
                                     ) intersectAdj 
    (*    val remainingColors = StringSet.difference( (Temp.Set.map (Temp.makestring) Frame.usableRegisters), colorSet) *)
        val remainingColors = StringSet.difference(tempSet2StrSet(Frame.usableRegisters), colorSet)
    in
        if StringSet.numItems(remainingColors) = 0 then (ErrorMsg.error 0 ("SPILL WARNING\n");Temp.makestring(Frame.ERROR))
        else hd (StringSet.listItems(remainingColors))
    end
   handle TG.NoSuchNode(_) =>(
      ErrorMsg.error 0 ("no such node exception in color one node");
      Temp.makestring(Frame.ERROR)
    ) 


    fun allocColors(iG,reducedIG, assignment:Temp.temp Temp.Map.map, stack:Temp.temp TG.node list) =
        let 
            fun allocColorsFoldFun(nextNode, (assSoFar, graphSoFar)) = 
                let val chosenColor = colorOneNode(iG, graphSoFar, assSoFar, nextNode)
                    val validColor = String.compare(chosenColor,Temp.makestring(Frame.ERROR))
                in
                    if validColor <> EQUAL then
                        let 
                            (*val (_) = print("in valid color\n")*)
                            val node = case StringMap.find (Frame.regsMap, chosenColor) of 
                                            SOME(t) => t
                                            | NONE => (ErrorMsg.error 0 "invalid color"; Frame.ERROR)
                            val newAss = Temp.Map.insert(assSoFar, TG.nodeInfo(nextNode), node)
                            val newGraph = TG.addNode(graphSoFar, TG.getNodeID(nextNode), TG.nodeInfo(nextNode))
                        in                    
                            (newAss, newGraph)
                        end
                    else
                    (
                        (* Actual Spill? *)
                        ErrorMsg.error 0 "GG need actual spill";
                        (assSoFar, graphSoFar)
                    )
                end
                    
        in
            let val (finalAss, finalGraph) = foldl allocColorsFoldFun (assignment, reducedIG) stack
            in
                finalAss
            end
        end
        handle TG.NoSuchNode(_) =>(
          ErrorMsg.error 0 ("no such node allocColors");
          Temp.Map.empty
        ) 
     
    fun color(iG, initial) =
        let
            val (iG',stack) = colorLoop(iG, [])
            (*val _ = Liveness.show(iG)*)
        in
            (allocColors(iG, iG', initial, stack), [])
        end
  
end