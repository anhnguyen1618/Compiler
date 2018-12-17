structure MipsFrame : FRAME = struct

datatype access = InFrame of int | InReg of Temp.temp
type frame = {name: Temp.label, formals: access list,
	      numLocals: int ref, curOffset: int ref}
datatype frag = PROC of {body: Tree.stm, frame: frame}
	      | STRING of Temp.label * string

type register = string

(* zero register *)
val R0 = Temp.newtemp()
		     
(* assembler temporary, reserved *)
val AT = Temp.newtemp() 

(* return value *)
val RV = Temp.newtemp() 
val V1 = Temp.newtemp()

(* function args *)
val A0 = Temp.newtemp() 
val A1 = Temp.newtemp()
val A2 = Temp.newtemp()
val A3 = Temp.newtemp()

(* Caller saved registers*)
val T0 = Temp.newtemp()
val T1 = Temp.newtemp()
val T2 = Temp.newtemp()
val T3 = Temp.newtemp()
val T4 = Temp.newtemp()
val T5 = Temp.newtemp()
val T6 = Temp.newtemp()
val T7 = Temp.newtemp()

(* Callee saved registers*)
val S0 = Temp.newtemp()
val S1 = Temp.newtemp()
val S2 = Temp.newtemp()
val S3 = Temp.newtemp()
val S4 = Temp.newtemp()
val S5 = Temp.newtemp()
val S6 = Temp.newtemp()
val S7 = Temp.newtemp()

val T8 = Temp.newtemp()
val T9 = Temp.newtemp()

(* reserved for kernel *)
val K0 = Temp.newtemp() 
val K1 = Temp.newtemp()

val GP = Temp.newtemp()
val SP = Temp.newtemp() (* stack pointer*)
val RA = Temp.newtemp() (* return address *)
val FP = Temp.newtemp()

val specialregs = [
    (RV, "$v0"),
    (V1, "$v1"),
    (R0, "$zero"),
    (AT, "$at"), 
    (K0, "$k0"),
    (K1, "$k1"),
    (GP, "$gp"),
    (SP, "$sp"),
    (FP, "$fp"),
    (RA, "$ra")
]
		      
val argregs = [
    (A0, "$a0"),
    (A1, "$a1"),
    (A2, "$a2"),
    (A3, "$a3")
]
		  
val calleesaves = [
    (S0, "$s0"),
    (S1, "$s1"),
    (S2, "$s2"),
    (S3, "$s3"),
    (S4, "$s4"),
    (S5, "$s5"),
    (S6, "$s6"),
    (S7, "$s7")
]
		      
val callersaves = [
    (T0, "$t0"),
    (T1, "$t1"),
    (T2, "$t2"),
    (T3, "$t3"),
    (T4, "$t4"),
    (T5, "$t5"),
    (T6, "$t6"),
    (T7, "$t7"),
    (T8, "$t8"),
    (T9, "$t9")
]


val calleesavedRegs = map (fn (x, _) => x) calleesaves

val callersaveRegs = map (fn (x, _) => x) callersaves
		     
val wordSize = 4
val START_OFF_SET= ~44

val addToNameTable = fn ((t, n), table) => Temp.Table.enter(table, t, n)
val registerMappings = specialregs @ argregs @ calleesaves @ callersaves
val tempMap = foldl addToNameTable Temp.Table.empty registerMappings

val registers = map #2 registerMappings

fun makestring tempMap temp = case Temp.Table.look(tempMap, temp) of
				  SOME x => x
				| NONE => Temp.makestring temp

fun string (lab, str) =
    Symbol.name(lab) ^ " .asciiz " ^ "\"" ^ str ^ "\"\n"
		       

fun newFrame {name: Temp.label, formals: bool list}: frame =
    let
	fun allocLocals ([], _) = []
	  | allocLocals (h::t, offset) = let val access = if h then InFrame(offset) else InReg(Temp.newtemp()) (*TODO: handle args number 4 up here*)
					    val newOffSet = if h then offset + wordSize else offset
					 in access :: allocLocals(t, newOffSet) end
	val formalAccesses = allocLocals (formals, 0)
    in
	{ name = name, formals = formalAccesses, numLocals = ref 0, curOffset = ref START_OFF_SET}
    end

(* val name: frame -> Temp.label = #name *)

val formals: frame -> access list = #formals

fun allocLocal f esc =
    let
	val {name = _, formals = formals, numLocals = numLocals, curOffset = curOffset} = f
	(*local var grows from high addr -> low addr *)
	fun decreaseOffset () = curOffset := !curOffset - wordSize
	val _ = print ("dec var: -> " ^ (if esc then "true" else "false") ^ "\n")
    in
	numLocals := !numLocals + 1;
	if esc then (decreaseOffset();
		     InFrame(!curOffset))
	else InReg(Temp.newtemp())
    end

(* Return address of variable in frame/register*)
fun exp (InFrame(offset)) frameAddress = Tr.MEM(Tr.BINOP(Tr.PLUS, frameAddress, Tr.CONST offset))
  | exp (InReg(temp)) _ = Tr.TEMP temp

fun externalCall (name, args) = Tr.CALL(Tr.NAME(Temp.namedlabel name), args)

fun procEntryExit1 (frame, stm) = stm

fun procEntryExit2 (frame, body) =
    body @ [Assem.OPER{assem = "",
		       src = [R0, RA, SP] @ calleesavedRegs, (*Recover those at the end of function*)
		       dst = [], jump = SOME([])}]

fun name (frame: frame): string = S.name(#name frame)
end


    
