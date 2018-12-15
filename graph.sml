structure LargeIntKey : ORD_KEY = 
struct 
type ord_key = int 
val compare = Int.compare
end


structure Graph :> GRAPH =
struct
  type node' = int
  type temp = Temp.temp

  structure NodeSet = SplaySetFn(LargeIntKey)
				
  structure NodeMap = HashTable


  datatype noderep = NODE of {succ: NodeSet.set, pred: NodeSet.set}	     

  val emptyNode = NODE{succ= NodeSet.empty, pred= NodeSet.empty}

  type graph = (node', noderep) NodeMap.hash_table (* int => noderep *)

  type node = graph * node'

  val key = ref 0

  fun getNewKey () = (key := !key + 1; !key);

  fun eq((_,a),(_,b)) = a=b

  fun augment (g: graph) (n: node') : node = (g,n)

  exception NOTFOUND
  fun newGraph (): graph = NodeMap.mkTable(HashString.hashString o Int.toString, op = ) (128, NOTFOUND)	  

  fun makeList (s: NodeSet.set) = NodeSet.listItems s

  exception NoSuchNode of node'
  fun getNode (g,nid) = case NodeMap.find g nid of
			   NONE => raise NoSuchNode(nid)
			 | SOME x=> x

  fun nodes (g: graph): node list =
      map (fn (k, _) => (g, k)) (NodeMap.listItemsi g)
      


  fun succ (g,i) = let val NODE{succ=s,...} = getNode(g, i)
		   in map (augment g) (makeList s) 
		   end
		       
  fun pred (g,i) = let val NODE{pred=p,...} = getNode(g, i)
                   in map (augment g) (makeList p) 
		   end
		       
  fun adj gi =
      let
	  val (g, i) = gi
	  val NODE{pred,succ} = getNode gi
	  val unionSet = NodeSet.union(succ,pred)
      in
	  map (augment g) (makeList unionSet) 
      end

  fun newNode g =
      let
	  val newKey = getNewKey()
	  val _ = NodeMap.insert g (newKey, emptyNode)
      in
	  (g, newKey)
      end
      
  exception GraphEdge
  fun check (g,g') = (* if g=g' then () else raise GraphEdge *) ()

  fun delete (i,j::rest) = if i=j then rest else j::delete(i,rest)
    | delete (_,nil) = raise GraphEdge

  fun diddle_edge change {from=(g:graph, i),to=(g':graph, j)} = 
      let val _ = check(g,g')
          val NODE{succ=si,pred=pi} = getNode(g, i)
          val _ = NodeMap.insert g (i,NODE{succ=change(si,j),pred=pi})
          val NODE{succ=sj,pred=pj} = getNode(g, j)
          val _ = NodeMap.insert g (j,NODE{succ=sj,pred=change(pj,i)})
       in ()
      end

  val mk_edge = diddle_edge NodeSet.add
  val rm_edge = diddle_edge NodeSet.delete

  structure Table = IntMapTable(type key = node
				fun getInt(g,n) = n)


  fun nodename(g,i:int) = "n" ^ Int.toString(i)

end

