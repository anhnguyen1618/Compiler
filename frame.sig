signature FRAME =
sig
    type frame
    type access
    type register = string
    datatype frag = PROC of {body: Tree.stm, frame: frame}
		  | STRING of Temp.label * string
    val FP : Temp.temp
    val RV : Temp.temp
    val RA : Temp.temp
    val SP : Temp.temp
    val newFrame: {name: Temp.label, formals: bool list} -> frame

    val string : Tree.label * string -> string
					    
    (*val name: frame -> Temp.label*)
    val formals: frame -> access list
    val allocLocal: frame -> bool -> access
    val wordSize: int
    val exp: access -> Tree.exp -> Tree.exp
    val externalCall: string * Tree.exp list -> Tree.exp
    val procEntryExit1: frame * Tree.stm -> Tree.stm

    val argregs: (Temp.temp * string) list
    val callersaveRegs: Temp.temp list

    val tempMap: register Temp.Table.table
    val registers: register list
    val name: frame -> string
end
    
