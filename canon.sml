signature CANON =
sig
    val linearize: Tree.exp -> Tree.stm * Tree.exp
end

structure Canon: CANON = struct

fun commute (Tr.Exp(Tr.CONST _), _) = true
  | commute (_, Tr.NAME _) = true
  | commute (_, Tr.CONST _) =  true
  | commute _ = false

val emptyStm = Tr.EXP(Tr.CONST 0)

fun linearize (Tr.BINOP(oper, l, r)): Tr.stm * Tr.exp =
    let
	val (leftSEQ, leftExp) = linearize l
	val (rightSEQ, rightExp) = linearize r
	val swappable = commute (rightSEQ, leftExp)
	val t = Temp.newlabel()
	val combinedSEQ = if swappable
			  then Tr.SEQ(leftSEQ,rightSEQ)
			  else Tr.SEQ(
				  Tr.SEQ(leftSEQ, Tr.MOVE(Tr.TEMP(t), leftExp)),
				  rightSEQ)
	val newLeft = if swappable then leftExp else Tr.TEMP t
	val combinedExp = Tr.BINOP(oper, newLeft, rightExp)
    in
	(combinedSEQ, combinedExp)
    end
	
  | linearize (Tr.MEM e) = linearize e
  | linearize (a as (Tr.TEMP e)) = (emptyStm, a)
  | linearize (Tr.ESEQ (stm, e)) =
    let
	val (seq, exp) = linearize e
	val newStm = linearizeStm stm
    in
	(Tr.SEQ(newStm, seq), exp)
    end
	
  | linearize (a as (Tr.NAME _)) = (emptyStm, a)
  | linearize (a as (Tr.CONST _)) = (emptyStm, a)
  | linearize (Tr.CALL (nameExp, args)) =
    let
	val (nameSEQ, funcName) = linearize nameExp
	val handleArgs = fn (cur, (stm, exps)) =>
			    let
				val (curStm, curExp) = linearize cur
				val swappable = commute (stm, curExp)
				val t = Temp.newlabel()
				val newStm = if swappable
					     then Tr.SEQ (curStm, stm)
					     else Tr.SEQ (curStm, Tr.SEQ(Tr.MOVE (Tr.TEMP(t), curExp), stm))
				val newExp = if swappable
					     then curExp::exps
					     else Tr.TEMP(t)::exps
			    in
				(newStm, newExp)
			    end
				
	val (stms, exps) = foldr handleArgs (emptyStm, nil) args
	val temp = Tr.TEMP(Temp.newlabel())
	val finalExp = Tr.CALL (funcName, exps)
	val finalStm = Tr.SEQ(Tr.SEQ(nameSEQ, stms), Tr.MOVE(temp, finalExp))
    in
	(finalStm, temp)
    end
	

and linearizeStm (Tr.SEQ(a, b)): Tr.stm =
    let
	val l = linearizeStm a
	val r = linearizeStm b	     
    in
	Tr.SEQ(l, r)
    end
  | linearizeStm (Tr.JUMP(e, l)) =
    let
	val (stm, exp) = linearize e
    in
	Tr.SEQ(stm, Tr.EXP(exp))
    end
  | linearizeStm (Tr.CJUMP(oper, a, b, l1, l2)) =
    let
	val (ls, le) = linearize a
	val (rs, re) = linearize b
	val swappable = commute (rs, le)
	val t = Temp.newtemp()
	val combinedSEQ = if swappable
			  then Tr.SEQ(ls, rs)
			  else Tr.SEQ(
				  Tr.SEQ(ls, Tr.MOVE(Tr.TEMP(t), le)),
				  rs)
	val newLeft = if swappable then le else Tr.TEMP t
    in
	Tr.CJUMP(oper, newLeft, re, l1, l2)
    end
	
  | linearizeStm (Tr.MOVE(a, b)) =
    let
	val (ls, le) = linearize a
	val (rs, re) = linearize b
	val swappable = commute (rs, le)
	val t = Temp.newtemp()
	val combinedSEQ = if swappable
			  then Tr.SEQ(ls, rs)
			  else Tr.SEQ(
				  Tr.SEQ(ls, Tr.MOVE(Tr.TEMP(t), le)),
				  rs)
	val newLeft = if swappable then le else Tr.TEMP t
    in
	Tr.SEQ(Tr.SEQ(ls, Tr.MOVE(newLeft, re)), rs)
    end
	
  | linearizeStm (Tr.EXP (e)) =
    let
	val (s, exp) = linearize e
    in
	T.SEQ(s, T.EXP(exp))
    end
	
		  

end
