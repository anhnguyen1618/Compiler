structure A = Absyn
structure T = Types
structure Err = ErrorMsg
		  
structure Semant = struct
    type venv = Env.enventry Symbol.table
    type tenv = Types.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}

    fun checkInt ({exp, ty = T.INT}, pos) = ()
      | checkInt ({exp, ty = _ }, pos) = Err.error pos "integer required"
	

    fun checkTypeEq (firstType, secondType, pos, errormsg) =
	if T.eq(firstType, secondType) then () else (Err.error pos errormsg)

			       
    fun transVar(vEnv: venv, tEnv: tenv, exp: Absyn.var) = ()

    fun transExp(vEnv: venv, tEnv: tenv, exp: Absyn.exp): expty =
	let
	    fun checkTypeOp ({left, oper, right, pos}) = (checkInt trExp(left); checkInt trExp(right); {exp = (), ty = T.INT})

	    fun checkSeqExp (xs) = foldl (fn ((exp, pos), _)) => trExp exp) {exp=(), ty=T.UNIT} xs

	    fun checkAssignExp ({var, exp, pos}) =
		let
		    val {ty = typeLeft} = transVar(vEnv, tEnv, var)
		    val {ty = typeRight} = trExp exp
		    val msg = "Can't assign type " ^ T.printTy(typeRight) ^ " to type " ^ T.printTr(typeLeft)
		in
		    (checkTypeEq (typeLeft, typeRight, pos, msg); { exp=(), ty=T.UNIT })
		end

	    fun checkIfExp ({test = testExp, then' = thenExp, else' = elseOption, pos}) =
		let
		    val typeThenExp = trExp thenExp
		in
		    (checkTypeEq (#ty (trExp testExp), T.INT, pos, "if test clause does not have type int");
		     case elseOption of
		       | NONE => typeThenExp
		       | SOME elseExp => (checkTypeEq (#ty typeThenExp, #ty (trExp elseExp), pos, "Mismatched types between then and else");
					  typeThenExp))
		end

	    fun checkWhileExp ({test, body, pos}) = (checkTypeEq (#ty (trExp test), T.INT, pos, "while test clause does not have type int");
						     checkTypeEq (#ty (trExp body), T.UNIT, pos, "body clause does not have type unit");
						     { exp = (), ty = T.UNIT })
		   
	    fun checkForExp ({lo, hi, body, pos}) = (checkTypeEq (#ty (trExp lo), T.INT, pos, "from-for clause does not have type int");
						     checkTypeEq (#ty (trExp high), T.UNIT, pos, "to-for clause does not have type int");
						     checkTypeEq (#ty (trExp body), T.UNIT, pos, "body of for clause does not have type unit");
						     { exp = (), ty = T.UNIT })
											   
	    fun checkLetExp ({decs, body, pos}) =
		let
		    val helper = fn (dec, {venv = vEnv, tenv = tEnv}) => transDec(vEnv, tEnv, dec)
		    val {venv = vEnv, tenv = tEnv} = foldl helper {venv = vEnv, tenv = tEnv} decs
		in
		    transExp (vEnv, tEnv, body)
		end
		    
							
	    fun trExp (A.VarExp(var)) = transVar(vEnt, tEnv, var)
	      | trExp (A.NilExp) = {exp = (), ty = T.NIL}
	      | trExp (A.IntExp(e)) = {exp = (), ty = T.INT}
	      | trExp (A.StringExp(e)) = {exp = (), ty = T.STRING}
	      | trExp (A.CallExp(e)) = {exp = (), ty = T.STRING} (* add later*)
	      | trExp (A.OpExp(e)) = checkTypeOp e
	      | trExp (A.RecordExp(e)) = {exp = (), ty = T.STRING} (* later *)
	      | trExp (A.SeqExp(e)) = checkSeqExp e
	      | trExp (A.AssignExp(e)) = checkAssignExp e
	      | trExp (A.IfExp(e)) = checkIfExp e
	      | trExp (A.WhileExp(e)) = checkWhileExp e
	      | trExp (A.ForExp(e)) = checkForExp e
	      | trExp (A.Break pos) = {exp = (), ty = T.STRING} (* later *)
	      | trExp (A.LetExp e) = checkLetExp e
	in	    
	    trExp exp
	end
	    


    fun transProg(exp: Absyn.exp): unit = 1
    
end
