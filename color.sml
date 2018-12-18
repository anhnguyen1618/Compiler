signature COLOR = sig
    structure Frame: FRAME
    type allocation = Frame.register Temp.Table.table

    val color: {interference: Liveness.igraph,
		initial: allocation,
		spillCost: Graph.node -> int,
		registers: Frame.register list} -> allocation * Temp.temp list
end


structure Color: COLOR = struct

structure Frame = MipsFrame
structure G = Graph
structure TT = Temp.Table
structure L = Liveness
type allocation = Frame.register Temp.Table.table


fun contains (xs, x) =
    case List.find (fn e => G.eq(e, x)) xs of
	SOME _ => true
      | NONE => false

				 

fun color {interference = L.IGRAPH{graph, tnode, gtemp, moves}, initial, spillCost, registers} =
    let	
	val selectedStack = ref []: G.node list ref

	val coloredNodeMapping = ref Temp.Table.empty: allocation ref

	val nodes = G.nodes graph
	val numRegs = List.length registers
	val _ = coloredNodeMapping := initial

	fun checkIsColored (node) =
	    let
		val temp =  gtemp(node)
	    in
		case TT.look(!coloredNodeMapping, temp) of
		    SOME x => true
		  | NONE => false
	    end

	fun getDegree node =
	    List.length(List.filter (fn x => not(contains(!selectedStack, x))) (G.adj node))

	fun findNodeToSimplify (nodes, numRegs) =
	    List.find (fn x => not (contains(!selectedStack, x))
			       andalso not(checkIsColored(x))
			       andalso getDegree(x) < numRegs) nodes 

	fun simplifyNode (node) = selectedStack := node::(!selectedStack)

	fun findNodeToSpill (nodes, numRegs) =
	    List.find (fn x => not(contains(!selectedStack, x))
			       andalso not(checkIsColored(x))
			       andalso getDegree(x) >= numRegs) nodes

	fun spillNode node =
	    selectedStack := node::(!selectedStack)
				       
	fun simplify () =
	    case findNodeToSimplify(nodes, numRegs) of
		SOME node => (simplifyNode(node); simplify())
	      | NONE => case findNodeToSpill(nodes, numRegs) of
			    SOME node => (spillNode (node); simplify())
			  | NONE => ()

	fun assignColor (node, spills) =
	    let
		val allColors = registers
		val curTemp = gtemp(node)
		fun f (cur, acc) = case TT.look(!coloredNodeMapping, gtemp(cur)) of
				       SOME x => x::acc
				     | NONE => acc
		    
		val neighborColors = foldl f [] (G.adj(node))
		(*val _ = print ("cur node "^ Temp.makestring(curTemp))
		val _ = print "\n----------------------\n"
		val _ = map (fn x => print (Temp.makestring(gtemp(x)) ^ " ")) (G.adj(node))
		val _ = print "\n----------------------\n"*)
		fun exclude (x) = case List.find (fn t => t = x) neighborColors of
				      SOME _ => false
				    | NONE => true
		val okcolor = List.find exclude allColors
	    in
		case okcolor of
		    SOME color => (coloredNodeMapping := TT.enter(!coloredNodeMapping, curTemp, color); (*print("node " ^ Temp.makestring(curTemp)^ ": "^ color ^"\n"); *) spills)
		  | NONE => (curTemp::spills)
	    end
	val _ = simplify()
	val spills = foldl assignColor [] (!selectedStack)
    in
	(!coloredNodeMapping, spills)
    end
    
end
