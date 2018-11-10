structure MipsFrame : FRAME = struct

datatype access = InFrame of int | InReg of Temp.temp
type frame = {name: Temp.label, formals: access list,
	      numLocals: int ref, curOffset: int ref}

val WORD_SIZE = 4
val START_OFF_SET= ~44

fun newFrame {name: Temp.label, formals: bool list}: frame =
    let
	fun allocLocals ([], _) = []
	  | allocLocals (h::t, offset) = let val access = if h then InReg(Temp.newtemp()) else InFrame(offset)
					    val newOffSet = if h then offset else offset + WORD_SIZE
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
	fun decreaseOffset () = curOffset := !curOffset - WORD_SIZE
    in
	numLocals := !numLocals + 1;
	if esc then InReg(Temp.newtemp())
	else (decreaseOffset(); InFrame(!curOffset))
    end
end
    
