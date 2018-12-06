structure MakeGraph:sig
	      val instrs2graph: Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
	  end =
struct

structure G = Graph
structure A = Assem

structure H = HashTable



val labelNodeMap : (string, G.node) H.hash_table = 
    H.mkTable(HashString.hashString, op = )


fun mapLabel ((instr as A.LABEL{assem = _, lab}), node: G.node) =
    (H.insert labelNodemap (S.name(lab), node); (instr, node))
  | mapLabel (instr, n) = (instr, n)

fun addNode (graph: G.graph) (instr: Assem.instr): (Assem.instr * G.node) =
    mapLabel(instr, G.newNode(graph))

exception labelNotFoundException;
fun findNode (label: Temp.label) =
    case H.find labelNodeMap label of
	SOME(n) => n
      | NONE => raise labelNotFoundException

fun addEdge (nodes: (Assem.instr * G.node) list) =
    let
	fun makeEdge cur next = G.mk_edge(cur, next)
	fun addJumpEdge (A.OPER{jump = SOME(labels),...}, node) =
	    map ((makeEdge node) o findNode) labels
	  | addJumpEdge (_, _) = ()
	fun helper ((cur as (curInstr, curNode))::(_, nextNode):: tl) =
	    (makeEdge curNode nextNode; addJumpEdge cur)
	  | helper (cur::[]) = addJumpEdge cur
	  | helper [] = () 
    in
	helper nodes
    end	

fun instrs2graph (instrs: Assem.instr list) =
    let
	val graph = G.newGraph()
	val instrNodes = map (addNode graph) instrs;
    in
	addEdge(graph, instrNodes)
    end
	

end
    
			 
