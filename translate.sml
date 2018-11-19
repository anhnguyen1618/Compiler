structure F = MipsFrame

signature TRANSLATE =
sig
    type exp
    type level
    type access

    val outermost: level
    val newLevel: {parent: level, name: Temp.label, formals: bool list} -> level
    val formals: level -> access list
    val allocLocal: level -> bool -> access

    val simpleVar : access * level -> exp
    val arrayDec : exp * exp -> exp
    val recordDec: exp list -> exp
    val intExp : int -> exp
    val fieldVar: exp * int -> exp
    val subscriptVar: exp * exp -> exp
    val whileExp: exp * exp -> exp
    val breakExp: Temp.label -> exp
    val ifExp: exp * exp * exp option -> exp
    val opExp: exp * A.oper * exp -> exp
    val funCallExp: level * level * label * exp list -> exp
end
    

structure Translate: TRANSLATE = struct

datatype level = TOP
	       | NESTED of {uniq: unit ref, parent: level, frame: F.frame}

datatype exp = 
         Ex of Tr.exp
	 | Nx of Tr.stm
	 | Cx of Temp.label * Temp.label -> Tr.stm

type access = level * F.access

val outermost = TOP

fun unEx (Ex e) = e
  | unEx (Nx (Tr.EXP e)) = e
  | unEx (Nx (Tr.SEQ(stm, Tr.SEQ e))) = Tr.ESEQ(stm, unEx (Nx(Tr.SEQ e)))
  | unEx (Nx (Tr.SEQ(stm, Tr.EXP e))) = Tr.ESEQ(stm, e)
  | unEx (Nx e) = Tr.ESEQ(e, Tr.CONST(0))
  | unEx (Cx f) =
    let
	val r = Temp.newtemp()
	val t = Temp.newlabel()
	val f = Temp.newlabel()
	val tempo = Tr.TEMP r
    in
	Tr.ESEQ(seq[f(t, f),
		    Tr.LABEL t,
		    Tr.MOVE(tempo, Tr.CONST 1),
		    Tr.LABEL f,
		    Tr.MOVE(tempo, Tr.CONST 0)], tempo)
    end

fun unNx (Ex e) = Tr.EXP e
  | unNx (Nx e) = e
  | unNx (Cx f) =
    let
	val r = Temp.newlabel()
    in
	seq[f(r, r), Tr.LABEL r]
    end

fun unCx (Ex Tr.CONST(1)) = (fn (t, f) => Tr.JUMP (Tr.NAME(t), [t]))
  | unCx (Ex Tr.CONST(0)) = (fn (t, f) => Tr.JUMP (Tr.NAME(f), [f]))
  | unCx (Ex e) = (fn (t, f) => Tr.CJUMP(Tr.EQ, e, Tr.CONST(1), t, f))
  | unCx (Nx _) = (Err.error 0 "Compiler error: unCx an Nx"; fn (a, b) => Tr.LABEL(Temp.newlabel()))
  | unCx (Cx f) = f
	


fun seq [] = Tr.EXP(Tr.CONST 0)
  | seq[stm] = stm
  | seq(stm::stms) = Tr.SEQ(stm,seq(stms))

fun newLevel {parent = p, name = _, formals = formals} =
    NESTED {uniq = ref (), parent = p,
	    frame = F.newFrame{ name = Temp.newlabel(), formals = true::formals}}

fun formals TOP = []
  | formals (NESTED e) = map (fn x => (NESTED(e), x)) ((#formals o #frame) e)

fun allocLocal lv esc =
    case lv of
	NESTED e => (lv, F.allocLocal (#frame e) esc)
      | TOP => (outermost, F.allocLocal (F.newFrame{name = Temp.newlabel(), formals = []}) esc)

fun generateSLChain (TOP, TOP, currentFP) = currentFP
  | generateSLChain (TOP, NESTED usedLevel, currentFP) = generateSLChain (TOP, #parent usedLevel, Tr.MEM (currentFP))
  | generateSLChain (NESTED decLevel, NESTED usedLevel, currentFP) =
    if (#unique decLevel) = (#unique usedLevel)
    then currentFP
    else generateSLChain (NESTED decLevel, #parent usedLevel, Tr.MEM (currentFP)) (*Add pointer offset here if needed*)
	

fun simpleVar ((decLevel, access), usedLevel) = Tr.exp access generateSLChain(decLevel, usedLevel, Tr.TEMP(F.FP))

fun arrayDec (sizeEx, initEx) =
    Ex (F.externalCall("tig_initArray", [unEx sizeExp, unEx initEx]))

fun recordDec (fields) =
    let
	val length = List.length fields
	val r = Temp.newtemp()
	val allocRecStm = Tr.MOVE(Tr.TEMP(r), Tr.CALL(Tr.NAME(Temp.namedlabel ("malloc")), Tr.CONST(length * F.wordSize)))
	fun calFieldAddr offset = Tr.MEM(Tr.BINOP(Tr.PLUS, Tr.TEMP(r), Tr.CONST(offset * F.wordSize)))
	fun createFieldStm (cur, (stms, offset)) = (Tr.MOVE(callFieldAddr(offset), unEx cur)::stms, offset - 1)
	val (fieldStms, _) = foldr createFieldStm ([], length -1) fields
	val finalLocationExp = Tr.TEMP(r)
    in
	Tr.ESEQ(seq(allocRecStm::fieldStms), finalLocationExp)
    end

fun intExp e = Ex (Tr.CONST e)

fun fieldVar (recordTemp, index) = Ex (Tr.MEM (Tr.BINOP(Tr.PLUS, unEx recordTemp, Tr.CONST(F.wordSize * index))))

fun subscriptVar (arrayTemp, indexExp) =
    case unEx indexExp of
      | Tr.CONST index => fieldVar(arrayTemp, index)
      | _ => (Err.error 0 "This should not happened because type of size is int";
	      intExp 0)

fun whileExp (testExp, bodyExp, done) =
    let
	val test = Temp.newlabel()
	val body = Temp.newlabel()
	val funControl = unCx testExp
	val bodyStm = unNx bodyExp
    in
	Nx (seq [
		 Tr.JUMP(Tr.NAME(test), [test]),
		 Tr.LABEL(test),
		 funControl(body, done),
		 Tr.LABEL(body),
		 bodyStm,
		 Tr.JUMP(Tr.NAME(test), [test]),
		 Tr.LABEL(done)
	   ])
    end

fun breakExp doneLabel =
    Nx Tr.JUMP(Tr.NAME(doneLabel), [doneLabel])

(* Come back and optimise Cx case later *)
fun ifExp (test, then', else') =
    let
	val t = Temp.newlabel()
	val f = Temp.newlabel()
	val r = Temp.newtemp()
	val join = Temp.newlabel()
	val func = unCx test
	val tempo = Tr.TEMP r
	val stms = case else' of
		     | SOME elseExp => seq [
					  func(t, f),
					  Tr.LABEL(t),
					  Tr.MOVE(tempo, unEx(then')),
					  Tr.JUMP(Tr.NAME(join), [join]),
					  Tr.LABEL(f),
					  Tr.MOVE(tempo, unEx(elseExp)),
					  Tr.JUMP(Tr.NAME(join), [join]),
					  Tr.LABEL(join)
				      ]
		     | NONE => seq [
				  func(t, f),
				  Tr.LABEL(t),
				  Tr.MOVE(tempo, unEx(then')),
				  Tr.JUMP(Tr.NAME(join), [join]),
				  Tr.LABEL(f),
				  Tr.JUMP(Tr.NAME(join), [join]),
				  Tr.LABEL(join)
			      ]
    in
	Ex Tr.ESEQ(stms, tempo)
    end



fun opExp (left, oper: A.oper, right) =
    let
	fun opCx(oper: Tr.binop) = Cx (fn (t, f) => Tr.CJUMP(oper, unEx(left), unEx (right), t, f))
	fun opEx(oper: Tr.biop) = Ex Tr.BINOP(oper, unEx (left), unEx (right))
    in
	case oper of
	  | A.PlusOp => opEx Tr.PLUS
	  | A.MinusOp => opEx Tr.MINUS
	  | A.TimesOp => opEx Tr.MUL 
	  | A.DivideOp => opEx Tr.DIV
	  | A.EqOp => opCx Tr.EQ
	  | A.NeqOp => opCx Tr.NE
	  | A.LtOp => opCx Tr.LT
	  | A.LeOp => opCx Tr.LE
	  | A.GtOp => opCx Tr.GT
	  | A.GeOp => opCx Tr.GE
    end

fun funCallExp (decLevel, usedLevel, label, args) =
    let
	val funAddr = generateSLChain(decLevel, usedLevel, Tr.TEMP(F.FP))
	val newArgs = funAddr::(map unEx args)
    in
	Ex Tr.CALL(label, newArgs)
    end
	

end
