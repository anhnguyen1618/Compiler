structure MakeGraph:sig
	      val instrs2graph: Assem.instr list -> Flow.flowgraph * Graph.node list
	  end =
struct

structure G = Graph
structure A = Assem
structure H = HashTable

val labelNodeMap : (string, G.node) H.hash_table = 
    H.mkTable(HashString.hashString, op = ) (42, Fail "not found")

val emptyTable = G.Table.empty

fun mapLabel ((instr as A.LABEL{assem = _, lab}), node: G.node) =
    (H.insert labelNodeMap (S.name(lab), node); (instr, node))
  | mapLabel (instr, n) = (instr, n)

fun addNode (graph: G.graph) (instr: Assem.instr): (Assem.instr * G.node) =
    mapLabel(instr, G.newNode(graph))

exception labelNotFoundException;
fun findNode (label: Temp.label) =
    case H.find labelNodeMap (S.name(label)) of
	SOME(n) => n
      | NONE => raise labelNotFoundException

fun addEdge (nodes: (Assem.instr * G.node) list) =
    let
	fun makeEdge cur next = G.mk_edge{from=cur, to=next}
	fun addJumpEdge (A.OPER{jump = SOME(labels),...}, node) =
	    map ((makeEdge node) o findNode) labels
	  | addJumpEdge (_, _) = []

	fun extractInfo ((A.OPER{dst, src,...}, node), (def, use, move)) =
	    (G.Table.enter(def, node, dst),
	     G.Table.enter(use, node, src),
	     G.Table.enter(move, node, false))
		
	  | extractInfo ((A.MOVE{dst, src,...}, node), (def, use, move)) =
	    (G.Table.enter(def, node, [dst]),
	     G.Table.enter (use, node, [src]),
	     G.Table.enter (move, node, true))
		
	  | extractInfo ((_, node), (def, use, move)) = 
	    (G.Table.enter(def, node, nil),
	     G.Table.enter(use, node, nil),
	     G.Table.enter(move, node, false))

	fun f (cur::next::tl, result) =
	    let
		val (_,curNode) = cur
		val (_, nextNode) = next
		val newResult = extractInfo(cur, result)
	    in
		makeEdge curNode nextNode;
		addJumpEdge cur;
		f (next::tl, newResult)
	    end
	  | f (cur::[], result) = (addJumpEdge cur; extractInfo(cur, result))
	  | f ([], result) = result
    in
	f (nodes, (emptyTable, emptyTable, emptyTable))
    end	

fun instrs2graph (instrs: Assem.instr list) =
    let
	val graph = G.newGraph()
	val instrNodes = map (addNode graph) instrs;
	val (def, use, move) = addEdge(instrNodes)
	val flowGraph = Flow.FGRAPH {
		control = graph,
		def = def,
		use = use,
		ismove = move
	    }
    in
	(flowGraph, G.nodes(graph))
    end
	

end
    
			 
