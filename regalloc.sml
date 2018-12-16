signature REG_ALLOC =
sig
    structure Frame: FRAME
    type allocation = Frame.register Temp.Table.table
    val alloc: Assem.instr list * Frame.frame -> Assem.instr list * allocation
end

structure RegAlloc: REG_ALLOC = struct
structure Frame = MipsFrame
structure F = MipsFrame
structure A = Assem
structure Tr = Tree
structure G = Graph
structure L = Liveness
structure C = Color

type allocation = F.register Temp.Table.table


fun rewriteProgram (spills: Temp.temp list, oldInstrs: A.instr list, frame: F.frame): A.instr list =
    let
	val mappingsAddress = ref Temp.Table.empty
	fun isSpill (t: Temp.temp) =
	    case List.find (fn x => x = t) spills of
		SOME _ => true
	      | NONE => false

	fun getAddress (temp: Temp.temp) =
	    case Temp.Table.look(!mappingsAddress, temp) of
		SOME x => x
	      | NONE =>
		let
		    (*This has to be true because we want to save to InFrame memory*)
		    val access = F.allocLocal(frame)(true); 
		    val addr = case F.exp(access)(Tr.TEMP(MipsFrame.FP)) of 
                              Tr.MEM(a) => a
                            | _ => raise Fail "spilled temp got assigned temp"
		in
		    addr
		end

	fun addMappingsAddress (t: Temp.temp, addr: Tr.exp) =
	    mappingsAddress := Temp.Table.enter(!mappingsAddress, t, addr)

	fun helper (temp: Temp.temp, handleSpill: Temp.temp * Tr.exp -> 'a, handleUnSpill) =
	    if isSpill(temp) then
		let
		    val addr = getAddress(temp)
		    val newTemp = Temp.newtemp()
		in
		    handleSpill (newTemp, addr)
		end
	    else handleUnSpill()
		
		    
			    
	fun rewriteInstr (instr as (A.OPER {dst = dsts,src = srcs, assem, jump}), newInstrs) =
	    let
		fun handleSpillSrcs (curTemp, (accSrcs, accInstrs)) =
		    let
			fun handleSpill (newTemp, addr) =
			    let
				val newInstrs = accInstrs
						@ Mipsgen.codegen frame (Tr.MOVE(Tr.TEMP(newTemp), Tr.MEM(addr)))
				val newSrcs = accSrcs @ [newTemp]
			    in
				(newSrcs, newInstrs)
			    end
			fun  handleUnSpill () = (accSrcs, accInstrs)
		    in
			helper(curTemp, handleSpill, handleUnSpill)
		    end

		fun handleSpillDsts (curTemp, (accDsts, accInstrs)) =
		    let
			fun handleSpill (newTemp, addr) =
			    let
				val newInstrs = accInstrs
						@ Mipsgen.codegen frame (Tr.MOVE(Tr.MEM(addr), Tr.TEMP(newTemp)))
				val _ = addMappingsAddress(curTemp, addr)
				val newDsts = accDsts @ [newTemp]
			    in
				(newDsts, newInstrs)
			    end
			fun handleUnSpill () = (accDsts, accInstrs)
		    in
			helper(curTemp, handleSpill, handleUnSpill) 
		    end

			    
		val (newSrcs, srcInstrs) = foldl handleSpillSrcs ([], []) srcs
		val (newDsts, dstInstrs) = foldl handleSpillDsts ([], []) dsts
		val newMainInstr = A.OPER {dst = newDsts, src = newSrcs, assem = assem, jump=jump}
	    in
		newInstrs @ srcInstrs @ [newMainInstr] @ dstInstrs
	    end
		
	  | rewriteInstr (A.MOVE {dst,src,assem}, newInstrs) =
	    if Temp.eq(dst, src)
	    then newInstrs
	    else
		let
		    fun handleSpillDst (newTemp, addr) =
			let
			    val _ = addMappingsAddress(dst, addr)
			    val accInstrs = newInstrs
					    @ Mipsgen.codegen frame (Tr.MOVE(Tr.TEMP(newTemp), Tr.TEMP(src)))
					    @ Mipsgen.codegen frame (Tr.MOVE(Tr.MEM(addr), Tr.TEMP(newTemp)))
			in
			    (newTemp, accInstrs)
			end
			     

		    fun handleSpillSrc (newTemp, addr) =
			(newTemp, newInstrs
				  @ Mipsgen.codegen frame (Tr.MOVE(Tr.TEMP(newTemp), Tr.MEM(addr)))
				  @ Mipsgen.codegen frame (Tr.MOVE(Tr.TEMP(dst), Tr.TEMP(newTemp))))
			    

		    val (srcTemp, srcInstrs) = helper (src, handleSpillSrc, fn () => (src, []))
		    val (dstTemp, dstInstrs) = helper (dst, handleSpillDst, fn () => (dst, []))
		    val newMainInstr = A.MOVE{dst = dstTemp, src = srcTemp, assem = assem}
		in
		    newInstrs @ srcInstrs @ [newMainInstr] @ dstInstrs
		end
		
	  | rewriteInstr (a, newInstrs) = newInstrs @ [a]
		
    in
	foldl rewriteInstr [] oldInstrs
    end
	


	
    
fun alloc (instrs: A.instr list, frame: F.frame): A.instr list * allocation =
    let
	val (flowGraph, _) = MakeGraph.instrs2graph(instrs)
	val (igraph, _) = Liveness.interferenceGraph(flowGraph)
	val _ = Liveness.show((), igraph)
	val (allocation, spills) = C.color{
		interference = igraph,
		initial = F.tempMap,
		spillCost = fn x => 1, (*actually calculate this shit*)
		registers = F.registers
	    }
    in
	if (List.length(spills) > 0)
	then
	    let
		val newInstrs = rewriteProgram (spills, instrs, frame)
	    in
		alloc(newInstrs, frame)
	    end
	else
	    (instrs, allocation)
    end	

end
				    
    
