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
    Ex (F.externalCall("tig_initArray", [unEx sizeExp, unEx initEx])
	       
end
