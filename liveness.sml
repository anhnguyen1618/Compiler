structure Liveness: sig
	      datatype igraph =
		       IGRAPH of {graph: Graph.graph,
				  tnode: Temp.temp -> Graph.node,
				  gtemp: Graph.node -> Temp.temp,
				  moves: (Graph.node * Graph.node) list}

	      val interferenceGraph: Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list)
	      val show: outstream * igraph -> unit
	  end =
struct

structure G = Graph
type liveSet = (*unit Temp.Table.table * *) Temp.temp list
type liveMap = liveSet G.Table.table

fun getTemps (table, node) =
    case G.Table.look(table, node) of
	SOME x => x
      | NONE => []

fun computeLiveMap {control, def, use, ismove}: liveMap =
    let
	val nodes = G.nodes control
	val shouldContinue = false

	fun unique (xs, alreadyAdded) =
	    let
		val f (cur, (added, result)) =
		    case Temp.table.look(added, cur) of
			SOME _ => (added, acc)
		      | NONE => (Temp.table.enter(added, cur, ()), cur::acc)
	    in
		fold f (alreadyAdded, []) xs
	    end
		
	fun computeInTemp (out, def, use) =
	    let
		val notAddable = fold ((cur, acc) => Temp.table.enter(acc, cur, ())) Temp.table.empty def
	    in
		unique(out@use, notAddable)
	    end
		
	fun compute (node: G.node, inMap: liveMap, outMap: liveMap): bool =
	    let
		val oldTempOut = getTemps(outMap, node)
		val oldTempIn = getTemps(inMap, node)
		val successors = G.succ node
		val defs = getTemps(def, node)
		val uses = getTemps(use, node)
				      
		val inUniqTempOut= fold (fn (succ, acc) => acc @ getTemps(inMap, succ)) [] successors
		val newTempOut = unique(inUniqTempout, Temp.table.empty)
		val newTempIn = computeInTemp (newTempOut, defs, uses)
		val hasChanges = (List.length(oldTempOut) <> List.length(newTempOut))
				 orelse (List.length(newTempIn) <> List.length(outTempIn))
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
	fun loop (_, outMap, false) = outMap
	  | loop (inMap, outMap, true) = loop(foldr f (inMap, outMap, false) nodes)
    in
	loop(G.Table.empty, G.Table.empty, true)
    end
	
    

fun computeIgraph liveTable =
    let
	val iGraph = G.newGraph()
			       
    in
	fillInIGraph(iGraph);
	addEdgeToIGraph(iGraph);
	IGRAPH {
	    graph = iGraph,
	    tnode = (),
	    gtemp = (),
	    move = ()
	}
    end
	
    

fun interferenceGraph (Flow.FGRAPH e) =
    let

	val liveTable = computeLiveMap(nodes, def, use)
	val igraph = computeIgraph(liveTable)
    in
	(igraph, (fn n => []))
    end
	

end
    
			
