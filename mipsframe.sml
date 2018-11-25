structure MipsFrame : FRAME = struct

datatype access = InFrame of int | InReg of Temp.temp
type frame = {name: Temp.label, formals: access list,
	      numLocals: int ref, curOffset: int ref}
datatype frag = PROC of {body: Tree.stm, frame: frame}
	      | STRING of Temp.label * string

val RV = Temp.newtemp()
val FP = Temp.newtemp()
val wordSize = 4
val START_OFF_SET= ~44

fun newFrame {name: Temp.label, formals: bool list}: frame =
    let
	fun allocLocals ([], _) = []
	  | allocLocals (h::t, offset) = let val access = if h then InFrame(offset) else InReg(Temp.newtemp())
					    val newOffSet = if h then offset + wordSize else offset
					 in access :: allocLocals(t, newOffSet) end
	val formalAccesses = allocLocals (formals, 0)
    in
	{ name = name, formals = formalAccesses, numLocals = ref 0, curOffset = ref START_OFF_SET}
    end

val name: frame -> Temp.label = #name

val formals: frame -> access list = #formals

fun allocLocal f esc =
    let
	val {name = _, formals = formals, numLocals = numLocals, curOffset = curOffset} = f
	(*local var grows from high addr -> low addr *)
	fun decreaseOffset () = curOffset := !curOffset - wordSize
    in
	numLocals := !numLocals + 1;
	if esc then InReg(Temp.newtemp())
	else (decreaseOffset(); InFrame(!curOffset))
    end

fun exp (InFrame(offset)) frameAddress = Tr.MEM(Tr.BINOP(Tr.PLUS, frameAddress, Tr.CONST offset))
  | exp (InReg(temp)) _ = Tr.TEMP temp

fun externalCall (name, args) = Tr.CALL(Tr.NAME(Temp.namedlabel name), args)

fun procEntryExit1 (frame, stm) = stm
				
end
    
