structure Liveness: sig
	      datatype igraph =
		       IGRAPH of {graph: Graph.graph,
				  tnode: Temp.temp -> Graph.node,
				  gtemp: Graph.node -> Temp.temp,
				  moves: (Graph.node * Graph.node) list}

	      val interferenceGraph: Flow.flowgraph -> igraph * (Graph.node -> Temp.temp list)
	      val show: 'a * igraph -> unit
	  end =
struct

structure G = Graph
type liveSet = (*unit Temp.Table.table * *) Temp.temp list
type liveMap = liveSet G.Table.table

datatype igraph =
	 IGRAPH of {graph: Graph.graph,
		    tnode: Temp.temp -> Graph.node,
		    gtemp: Graph.node -> Temp.temp,
		    moves: (Graph.node * Graph.node) list}

structure H = HashTable
val tempNodeMap : (Temp.temp, G.node) H.hash_table = 
    H.mkTable(HashString.hashString o Int.toString, op = ) (42, Fail "not found")

val nodeTempMap = ref (G.Table.empty : Temp.temp G.Table.table)

val flowNodeTempMap = ref (G.Table.empty: (Temp.temp list) G.Table.table)

fun getGlobalTempsFromFlowNode node =
    case G.Table.look (!flowNodeTempMap, node) of
	SOME x => x
      | NONE => []

exception labelNotFoundException;
fun getIgraphNode (temp: Temp.temp): G.node =
    case H.find tempNodeMap temp of
	SOME x => x
      | NONE => raise labelNotFoundException

fun getTempFromIGraphNode (node: G.node): Temp.temp =
    case G.Table.look (!nodeTempMap, node) of
	SOME x => x
      | NONE => raise labelNotFoundException
    

fun getTemps (table, node: G.node): Temp.temp list =
    case G.Table.look(table, node) of
	SOME x => x
      | NONE => []

fun getTempsFromFlowNode (def, use, flowNode): Temp.temp list =
    getTemps(def, flowNode) @ getTemps(use, flowNode)

fun unique (xs: Temp.temp list, notAddable): Temp.temp list =
    let
	fun f (cur, (notAddable, result)) =
	    case Temp.Table.look(notAddable, cur) of
		SOME _ => (notAddable, result)
	      | NONE => (Temp.Table.enter(notAddable, cur, ()), cur::result)
	val (_, temps) = foldl f (notAddable, []) xs 		    
    in
	temps
    end

fun computeLiveMap {control: G.graph, def, use, ismove}: liveMap =
    let
	val flowNodes = G.nodes control
	val shouldContinue = false
		
	fun computeInTemp (outTemps, defTemps, useTemps): Temp.temp list =
	    let
		val notAddable = foldl (fn (cur, acc) => Temp.Table.enter(acc, cur, ())) Temp.Table.empty defTemps
	    in
		unique(unique(outTemps, notAddable)@ useTemps, Temp.Table.empty)
	    end
		
	fun compute (node: G.node, inMap: liveMap, outMap: liveMap) =
	    let
		val oldTempOut = getTemps(outMap, node)
		val oldTempIn = getTemps(inMap, node)
		val successors = G.succ node
		val defs = getTemps(def, node)
		val uses = getTemps(use, node)
				      
		val inUniqTempOut= foldl (fn (succ, acc) => acc @ getTemps(inMap, succ)) [] successors
		val newTempOut = unique(inUniqTempOut, Temp.Table.empty)
		val newTempIn = computeInTemp (newTempOut, defs, uses)
		val hasChanges = (List.length(oldTempOut) <> List.length(newTempOut))
				 orelse (List.length(newTempIn) <> List.length(oldTempIn))
	    in
		(G.Table.enter(inMap, node, newTempIn),
		 G.Table.enter(outMap, node, newTempOut),
		 hasChanges)
	    end
		
	fun f (cur, (inMap, outMap, continue)) =
	    let
		val (newInMap, newOutMap, hasChanges) = compute(cur, inMap, outMap)
	    in
		(newInMap, newOutMap, continue orelse hasChanges)
	    end
		
	fun loop (_, outMap, false): liveMap = outMap
	  | loop (inMap, outMap, true) = loop(foldr f (inMap, outMap, false) flowNodes)
    in
	loop(G.Table.empty, G.Table.empty, true)
    end
	
fun extractAllTemps (def, use, nodes: G.node list): Temp.temp list =
    let
	fun extract (node, acc) =
	    let
		val temps = getTempsFromFlowNode(def, use, node)
		val _ = flowNodeTempMap := G.Table.enter(!flowNodeTempMap, node, temps)
	    in
		temps @ acc
	    end
		
	val allTemps = foldl extract [] nodes
    in
	unique(allTemps, Temp.Table.empty)
    end

fun addNodeToIGraph (graph: G.graph, temps: Temp.temp list): unit list =
    let
	fun addNode temp =
	    let
		val newNode = G.newNode(graph)
		val _ = H.insert tempNodeMap (temp, newNode);
		val _ = nodeTempMap := G.Table.enter (!nodeTempMap, newNode, temp);
	    in	
		()
	    end
    in
	map addNode temps
    end

fun makeEdge cur next = G.mk_edge{from=cur, to=next}
	
fun addEdgeToIGraphAndComputeMoveList (
    flowNodes: G.node list,
    def: liveMap,
    use: liveMap,
    ismove: bool G.Table.table,
    liveTable: liveMap): (G.node * G.node) list =
    let
	fun f (node, acc) =
	    (let
		val _ = (case (getTemps(def, node), getTemps(liveTable, node)) of
			    (defTemp::[], outs) =>
			    let
				val curNode = getIgraphNode (defTemp)
				val addEdge = makeEdge curNode
				val _ = map (addEdge o getIgraphNode) outs;
			    in
				()
			    end
			  | (_, _) => ())
	    in
		case (G.Table.look(ismove, node), getTemps(def, node), getTemps(use, node)) of
		    (SOME (true), [dstTemp], [srcTemp]) => (getIgraphNode(dstTemp), getIgraphNode(srcTemp))::acc
		  | (_, _, _) => acc
	    end)
    in
	foldl f [] flowNodes
    end

fun computeIgraph (
    liveTable: liveMap,
    flowNodes: G.node list,
    def: liveMap,
    use: liveMap,
    temps: Temp.temp list,
    ismove: bool G.Table.table)  =
    let
	val igraph = G.newGraph()
	val _ = addNodeToIGraph (igraph, temps);
	val moveList = addEdgeToIGraphAndComputeMoveList(flowNodes, def, use, ismove, liveTable);
    in
	IGRAPH {
	    graph = igraph,
	    tnode = getIgraphNode,
	    gtemp = getTempFromIGraphNode,
	    moves = moveList
	}
    end
	

fun interferenceGraph (Flow.FGRAPH(e as {control, def, use, ismove})) =
    let
	val flowNodes = G.nodes control
	val temps = extractAllTemps(def, use, flowNodes)
	val liveTable = computeLiveMap(e)
	val igraph = computeIgraph(liveTable, flowNodes, def, use, temps, ismove)
    in
	(igraph, getGlobalTempsFromFlowNode)
    end

fun show (stream, graph) = ()
	

end
    
			
