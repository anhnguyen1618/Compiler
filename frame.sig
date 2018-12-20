signature FRAME =
sig

    type access
    type register = string
    type frame
	     
    val registers: register list
    val tempMap: register Temp.Table.table
    val wordSize: int
    val initialOffset: int
		      
    val externalCall: string * Tree.exp list -> Tree.exp

    val newFrame: {name: Temp.label, formals: bool list} -> frame
    val formals: frame -> access list
    (*val name: frame -> Temp.label*)
    val allocLocal: frame -> bool -> access
    val string : Tree.label * string -> string
					    
    datatype frag = PROC of {body: Tree.stm, frame: frame}
		  | STRING of Temp.label * string
    val FP : Temp.temp
    val RV : Temp.temp
    val RA : Temp.temp
    val SP : Temp.temp

    val exp: access -> Tree.exp -> Tree.exp

    val procEntryExit1: frame * Tree.stm -> Tree.stm
    val procEntryExit2: frame * Assem.instr list -> Assem.instr list * int
    val procEntryExit3: frame * Assem.instr list * int ->
			{prolog: string, body: Assem.instr list, epilog: string}

    val argregs: (Temp.temp * string) list
    val argRegs: Temp.temp list
    val callersaveRegs: Temp.temp list
    val makestring: register Temp.Table.table -> Temp.temp -> string
    val name: frame -> string
end
    
