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
end
    

structure Translate: TRANSLATE = struct

datatype level = TOP
	       | NESTED of {uniq: unit ref, parent: level, frame: F.frame}

type exp = unit

type access = level * F.access

val outermost = TOP

fun newLevel {parent = p, name = _, formals = formals} =
    NESTED {uniq = ref (), parent = p,
	    frame = F.newFrame{ name = Temp.newlabel(), formals = true::formals}}

fun formals TOP = []
  | formals (NESTED e) = map (fn x => (NESTED(e), x)) ((#formals o #frame) e)

fun allocLocal lv esc =
    case lv of
	NESTED e => (lv, F.allocLocal (#frame e) esc)
      | TOP => (outermost, F.allocLocal (F.newFrame{name = Temp.newlabel(), formals = []}) esc)
	       
end
