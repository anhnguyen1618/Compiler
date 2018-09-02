type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp

val test = EseqExp(PrintStm [IdExp "a", NumExp 4, NumExp 5, NumExp 6], NumExp 4)
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

exception NotFound
	    
fun maxargs e =
    case e of
	CompoundStm(a, b) =>
	let
	    val first_max = maxargs a
	    val second_max = maxargs b
	in
	    if first_max > second_max then first_max else second_max
	end
      | AssignStm(_, e) => maxargsExp e
      | PrintStm e =>
	let
	    val list_length = List.length e
	    val numbers = List.map (fn ex => maxargsExp ex) e
	    val cur_max = List.foldl (fn (c, acc) => if c > acc then c else acc) list_length numbers
	in
	    cur_max
	end

and maxargsExp e =
    case e of
	OpExp(a, _, b) =>
	let
	    val first_max = maxargsExp a
	    val second_max = maxargsExp b
	in
	    if first_max > second_max then first_max else second_max
	end
      | EseqExp(st, exp) =>
	let
	    val first_max = maxargs st
	    val second_max = maxargsExp exp
	in
	    if first_max > second_max then first_max else second_max
	end
      | _ => 0

fun lookup(table, str) =
    let	
	val result = List.find (fn (k, _) => k = str) table
    in
	case result of
	    SOME(_, v) => v
	 | _ => raise NotFound
    end

fun interpStm (st, table) =
    case st of
	CompoundStm(a , b) =>
	let
	    val new_table = interpStm(a, table)
	    val newer_table = interpStm(b, new_table)
	in
	    newer_table
	end
      | AssignStm(str, e) =>
	let
	    val (result, new_table) = interpExp(e, table)
	in
	    (str, result) :: new_table
	end
      | PrintStm ls =>
	List.foldl (fn (cur, acc) =>
		       let
			   val (result, new_table) = interpExp(cur, acc)
			   val _ = print (Int.toString(result) ^ "\n")
		       in
			   new_table
		       end) table ls

and interpExp (e, table) =
    case e of
      EseqExp(st, ex) =>
	let
	    val new_table = interpStm(st, table)
	in
	    interpExp(ex, new_table)
	end
      | IdExp str => (lookup(table, str), table)
      | NumExp i => (i, table)
      | OpExp (a, ops, b) =>
	let
	    val (resultA, tableA) = interpExp(a, table)
	    val (resultB, tableB) = interpExp(b, tableA)
	    val result =case ops of
		Plus => resultA + resultB
	      | Minus => resultA - resultB
	      | Div => resultA div resultB
	      | Times => resultA * resultB
	in
	    (result, tableB)
	end

fun interp st =
    interpStm(st, [])


type key = string
datatype tree = LEAF | TREE of tree * key * tree

val empty = LEAF
fun insert(key, LEAF) = TREE(LEAF, key, LEAF)
  | insert (key, TREE(l, k, r)) =
    if key < k
    then TREE(insert(key, l), k, r)
    else if key > k
    then TREE(l, k, insert(key, r))
    else TREE(l, key, r)

fun member(key, LEAF) = false
  | member (key, TREE(l, k, r)) =
    if key = k then true
    else if key > k
    then member(key, r)
    else member(key, l)

datatype 'a tree = LEAF | TREE of tree * (key * 'a) * tree
fun insert(LEAF, key, value) = TREE(LEAF, (key, value), LEAF)
  | insert(TREE(l, (k, v), r), key, value) =
    if k < key then TREE(l, (k,v), insert(r, key, value))
    else if k > key then TREE(insert(l, key, value), (k,v), r)
    else TREE(l, (k, value), r)
							  
fun lookup(LEAF, key) = Nil
  | lookup (TREE(l, (k, v), r), key) =
    if key = k then v
    else if key < k then lookup(l, key)
    else lookup(r, key)

	       
	    
		 
	    

