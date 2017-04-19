structure IntFuncGraph = FuncGraph(type ord_key=int
                                        fun compare(x,y) = 
                                            if x<y then LESS
                                            else(
                                                if x>y then GREATER
                                                else EQUAL
                                                )
                                        )

structure TempFuncGraph = FuncGraph(type ord_key=string
                                        fun compare(x,y) = String.compare(x,y))

structure StringMap = SplayMapFn(type ord_key=string
                                 fun compare(x, y) = String.compare(x, y))

(*functor SplayMapFn (ORD_KEY) : ORD_MAP*)
                                        
structure FlowGraph =
    struct
    structure LiveSet = SplaySetFn(type ord_key = Temp.temp fun compare(x,y)=String.compare(Temp.makestring x, Temp.makestring y))
    type liveSet = LiveSet.set
    type nodeDataType = ( liveSet * liveSet * string * bool * liveSet * liveSet)
    type graph = nodeDataType IntFuncGraph.graph
    type iDataType = (Temp.temp)
    type iGraph = iDataType TempFuncGraph.graph
end