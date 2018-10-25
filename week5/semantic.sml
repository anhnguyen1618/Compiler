structure A = Absyn
structure T = Types
structure E = Env
structure Err = ErrorMsg
structure S = Symbol
		  
structure Semant = struct
    type venv = Env.enventry Symbol.table
    type tenv = Types.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}

    fun checkInt ({exp, ty = T.INT}, pos) = ()
      | checkInt ({exp, ty = _ }, pos) = Err.error pos "integer required"
	

    fun checkTypeEq (firstType, secondType, pos, errormsg) =
	if T.eq(firstType, secondType) then () else (Err.error pos errormsg)

			       
    fun transVar (vEnv: venv, tEnv: tenv, exp: Absyn.var) =
	let
	    fun checkSimpleVar (s, pos) =
		case S.look(tEnv, s) of
		    SOME e => {exp = (), ty: e}
		  | NONE => (Err.error pos ("This valuable '" ^ S.name(s) ^"' has not been declared"); {exp = (), ty: T.INT})

	    fun checkFieldVar (var, s, pos) =
		let
		    val typeOfVar = #ty trVar var
		in
		    case typeOfVar of
		      | T.RECORD (tys, _) =>
			let
			    val matchedField = List.find (fn (symbol, _) => S.eq(s, symbol)) tys
			in
			    case matchedField of
				SOME (_, ty) => {exp = (), ty = ty}
			      | NONE => {exp = (), ty = T.NIL}
			end
		      | _ => (Err.error pos ("Can't access property '" ^ S.name(s) ^ "'of non-record type"); {exp = (), ty = T.NIL})
		end

	    fun checkFieldVar (var, s, pos) =
		case #ty trVar var of
		      | T.ARRAY (ty, _) => {exp = (), ty = ty}
		      | _ => (Err.error pos ("Can't access index of '" ^ S.name(s) ^ "'of non-array type"); {exp = (), ty = T.NIL})		    
		    
	    fun trVar (A.SimpleVar e) = checkSimpleVar e
	      | trVar (A.FieldVar e) = checkFieldVar e
	      | trVar (A.SubscriptVar e) = checkArrayVar e
	in
	end
	    

    fun transExp(vEnv: venv, tEnv: tenv, exp: Absyn.exp): expty =
	let
	    fun checkTypeOp ({left, oper, right, pos}) = (checkInt trExp(left); checkInt trExp(right); {exp = (), ty = T.INT})

	    fun checkFnCallExp ({func, args, pos}) =
		let
		    val helper = fn ((exp, ty), _) =>
				    let
					val actualType = #ty (trExp exp)
				    in
					checkTypeEq (
					    ty,
					    actualType,
					    pos,
					    "Mismatched types of function args. Expect: " ^ T.printTr(ty) ^ " . Received: "^ T.printTr(actualType)
					)
				    end
		in
		    case S.look(vEnv, func) of
			SOME (E.FunEntry({formal, result})) => (
			  foldl helper {exp = (), ty = T.UNIT}  ListPair.zip(args, formal);
			  {exp = (), ty = result})
		      | SOME _ => Err.error pos (S.name(func) ^ " does not have type function")
		      | NONE => Err.error pos ("Function " ^ S.name(func) ^ " can't be found")
		end

	    fun checkRecordExp (fields, typ, pos) =
		let
		    val fieldExps = map (fn (symbol, exp, pos) => (symbol, trExp exp, pos)) fields
		in
		    case S.look(tEnv, typ) of
			SOME (T.RECORD (types, refer)) =>
			let
			    fun f [] = {exp = (), ty = T.UNIT}
			      | f ((s, t)::tl) =
				let
				    val matchedField = List.find (fn (symbol, _, _) => S.eq(s, symbol)) fieldExps
				in
				    ((case matchedField of
					  SOME(symbol, typeExp, pos) =>
					  checkTypeEq (
					      t,
					      typeExp,
					      pos,
					      "Mismatched types of fields property. Expect: " ^ T.printTr(t) ^ " . Received: "^ T.printTr(typeExp)
					  )
					| NONE  => Err.error pos ("Field " ^ S.name(symbol) ^ " is missing"));
				     f(tl))
				end
			in
			    (f types; {exp = (), ty = T.RECORD (types, refer)})
			end
			    
		      | SOME _ => (Err.error pos (S.name(typ) ^ " does not have type record"); {exp = (), ty = T.RECORD ([], ref ())})
		      | NONE => (Err.error pos ("type " ^ S.name(typ)  ^ " can't be found"); {exp = (), ty = T.RECORD ([], ref ())})
		end

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
		         NONE => typeThenExp
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

	    fun checkArrayExp ({typ, size, init, pos}) =
		case S.look(tEnv, typ) of
		    SOME (T.ARRAY(ty, unique)) =>
		    (checkTypeEq (#ty (trExp size), T.INT, pos, "Size of array must have type " ^ T.printTr(T.INT));
		     checkTypeEq (#ty (trExp exp), ty, pos, "Initialize value of array does not have type " ^ T.printTr(ty));
		     {exp = (), ty = T.ARRAY(ty, unique)})
		  | SOME _ => Err.error pos (S.name(typ) ^ " does not have type array")
		  | NONE => Err.error pos ("Type " ^ S.name(typ) ^ " could not be found")
		
		    
							
	    fun trExp (A.VarExp(var)) = transVar(vEnt, tEnv, var)
	      | trExp (A.NilExp) = {exp = (), ty = T.NIL}
	      | trExp (A.IntExp(e)) = {exp = (), ty = T.INT}
	      | trExp (A.StringExp(e)) = {exp = (), ty = T.STRING}
	      | trExp (A.CallExp(e)) = checkFnCallExp e
	      | trExp (A.OpExp(e)) = checkTypeOp e
	      | trExp (A.RecordExp(e)) = checkRecordExp e
	      | trExp (A.SeqExp(e)) = checkSeqExp e
	      | trExp (A.AssignExp(e)) = checkAssignExp e
	      | trExp (A.IfExp(e)) = checkIfExp e
	      | trExp (A.WhileExp(e)) = checkWhileExp e
	      | trExp (A.ForExp(e)) = checkForExp e
	      | trExp (A.Break pos) = {exp = (), ty = T.STRING} (* later *)
	      | trExp (A.LetExp e) = checkLetExp e
	      | trExp (A.ArrayExp e) = checkArrayExp e
	in	    
	    trExp exp
	end
	    


    fun transProg(exp: Absyn.exp): unit = 1
    
end
