
structure E = Env
		  
structure Semant = struct
    type venv = E.enventry Symbol.table
    type tenv = Types.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}

    fun checkInt ({exp, ty = T.INT}, pos) = ()
      | checkInt ({exp, ty = _ }, pos) = Err.error pos "integer required"

    fun checkTypeEq (firstType, secondType, pos, errormsg) =
	if T.eq(firstType, secondType) then () else (Err.error pos errormsg)

    fun	transTy (tEnv, ty) =
	let
	    fun lookUpType (s, p) =
		case S.look(tEnv, s) of
		    SOME t => t
		  | NONE => (Err.error p ("Type " ^ S.name(s) ^ " has not been declared"); T.NIL)
	    fun mapFieldToRecord {name, typ, pos, escape = _} = (name, lookUpType(typ, pos))
		    
	    fun checkRecord fields = T.RECORD (map mapFieldToRecord fields, ref ())

	    fun checkNameTy e = lookUpType e

	    fun checkArrayTy e = T.ARRAY(lookUpType e, ref())
			 
	    fun trTy (A.NameTy e) = checkNameTy e
	      | trTy (A.RecordTy e) = checkRecord e
	      | trTy (A.ArrayTy e) = checkArrayTy e
	in
	    trTy ty
	end
	    
    and transDec (vEnv: venv, tEnv: tenv, exp: Absyn.dec) =
	let
	    fun checkVarDec ({name, typ, init, pos, escape = _}) =
		let
		    val {exp = _, ty} = transExp(vEnv, tEnv, init)
		in
		    case typ of
			SOME (s, p) => (case S.look(tEnv, s) of
					   SOME t => if T.eq(t, ty)
						     then {venv = S.enter(vEnv, name, E.VarEntry{ty = ty}), tenv = tEnv}
						     else (Err.error p ("can't assign exp type " ^ T.name(ty) ^ " to type " ^ S.name(s));
							   {venv = vEnv, tenv = tEnv})
					 | NONE => (Err.error pos ("Type " ^ S.name(s) ^ " has not been declared"); {venv = vEnv, tenv = tEnv}))
		      | NONE => {venv = S.enter(vEnv, name, E.VarEntry{ty = ty}), tenv = tEnv}
		end

	    fun checkTypeDec (xs) =
		let
		    fun f ({name, ty, pos}, {venv, tenv}) = {tenv = S.enter(tenv, name, transTy(tenv, ty)), venv = venv}
		in
		    foldl f {venv = vEnv, tenv = tEnv} xs
		end

	    fun checkFuncDec (fs) =
		let
		    fun lookTypeUp (s, p) =
			case S.look(tEnv, s) of
			    SOME t => t
			  | NONE => (Err.error p ("Type " ^ S.name(s) ^ "has not been declared"); T.NIL)

		    fun getTypeForResult (SOME e) = lookTypeUp e
		      | getTypeForResult (NONE) = T.UNIT

		    fun getType ({name, escape = _, typ, pos}) = {name = name, ty = lookTypeUp(typ, pos)}

		    fun checkResultType (expectType, resultType) =
			if T.eq(expectType, T.UNIT) then true
			else T.eq(expectType, resultType)

		    fun addFuncHeaders ({name, params, result, body, pos}, acc) =
			let
			    val typeList = map (#ty o getType) params
			    val resultType = getTypeForResult result
			in
			    S.enter(acc, name, E.FunEntry{formals = typeList, result = resultType})
			end
						
		    fun addNewFuncEntry ({name, params, result, body, pos}, curVenv) = 
																   
			let
			    val typeList = map getType params
			    val resultType = getTypeForResult result
			    val addParamsToBody = fn ({name, ty}, temp) => S.enter(temp, name, E.VarEntry({ty = ty}))

			    val bodyVenv = foldl addParamsToBody curVenv typeList
			    val {exp = _, ty = tyBody } = transExp(bodyVenv, tEnv, body)
			in
			    (if checkResultType(resultType, tyBody) then ()
			     else (Err.error pos ("return type " ^ T.name(tyBody) ^ " does not match with " ^ T.name(resultType)));
			    curVenv)
			end			    

		    val baseEnv = foldl addFuncHeaders vEnv fs
			    	
		    val newvEnv = foldl addNewFuncEntry baseEnv fs
		in
		    {venv = newvEnv, tenv = tEnv}
		end
		    
	    fun trDec (A.VarDec e ) = checkVarDec e
	      | trDec (A.TypeDec e) = checkTypeDec e
	      | trDec (A.FunctionDec e) = checkFuncDec e
		
	in
	    trDec exp
	end
			       
    and transVar (vEnv: venv, tEnv: tenv, exp: Absyn.var) =
	let
	    fun checkSimpleVar (s, pos) =
		case S.look(vEnv, s) of
		    SOME(E.VarEntry({ty})) => {exp = (), ty = (*actual*) ty}
		  | SOME _ => {exp = (), ty = T.NIL}
		  | NONE => (Err.error pos ("valuable '" ^ S.name(s) ^"' has not been declared"); {exp = (), ty = T.NIL})

	    and checkFieldVar (obj, s, pos) =
		let
		    val typeOfObj = #ty (trVar obj)
		in
		    case typeOfObj of
			T.RECORD (tys, _) =>
			let
			    val matchedField = List.find (fn (symbol, _) => S.eq(s, symbol)) tys
			in
			    case matchedField of
				SOME (_, ty) => {exp = (), ty = ty}
			      | NONE => (Err.error pos ("Property '" ^ S.name(s) ^ "' does not exist on type '" ^ T.name(typeOfObj) ^ "'"); {exp = (), ty = T.NIL})
			end
		      | _ => (Err.error pos ("Can't access property '" ^ S.name(s) ^ "' of type '"^ T.name(typeOfObj) ^ "'"); {exp = (), ty = T.NIL})
		end

	    and checkArrayVar (var, sizeExp, pos) =
		case #ty (trVar var) of
		    T.ARRAY (ty, _) =>
		    let
			val {exp = _, ty = sizety} = transExp(vEnv, tEnv, sizeExp)
						    
		    in
			if T.eq(sizety, T.INT)
			then {exp = (), ty = ty}
			else (Err.error pos "index of array is not int"; {exp = (), ty = T.NIL})
		    end
		  | _ => (Err.error pos ("Can't access member of non-array type"); {exp = (), ty = T.NIL})		    
		    
	    and trVar (A.SimpleVar e) = checkSimpleVar e
	      | trVar (A.FieldVar e) = checkFieldVar e
	      | trVar (A.SubscriptVar e) = checkArrayVar e
	in
	    trVar exp
	end


    and transExp (vEnv: venv, tEnv: tenv, exp: Absyn.exp): expty =
	let
	    fun checkTypeOp ({left, oper, right, pos}) = (
		checkInt (trExp(left), pos);
		checkInt (trExp(right), pos);
		{exp = (), ty = T.INT})

	    and checkFnCallExp ({func, args, pos}) =
		let
		    fun checkParam ((exp, ty), _) =
			let
			    val actualType = #ty (trExp exp)
			in
			    checkTypeEq (
				ty,
				actualType,
				pos,
				"Mismatched types of function args. Expect: " ^ T.name(ty) ^ " . Received: "^ T.name(actualType)
			    )
			end
		in
		    case S.look(vEnv, func) of
			SOME (E.FunEntry({formals, result})) => (
			 (foldl checkParam () (ListPair.zip(args, formals)));
			 {exp = (), ty = result})
		      | SOME _ => (Err.error pos (S.name(func) ^ " does not have type function"); {exp = (), ty = T.UNIT})
		      | NONE => (Err.error pos ("Function " ^ S.name(func) ^ " can't be found"); {exp = (), ty = T.UNIT})
		end

	    and checkRecordExp {fields, typ, pos} =
		let
		    val fieldExps = map (fn (symbol, exp, pos) => (symbol, #ty (trExp exp), pos)) fields
		in
		    case S.look(tEnv, typ) of
			SOME (T.RECORD (types, refer)) =>
			let
			    fun checkFields [] = ()
			      | checkFields ((s, t, p)::tl) =
				let
				    val matchedField = List.find (fn (symbol, _) => S.eq(s, symbol)) types
				in
				    ((case matchedField of
					  SOME(symbol, typeExp) =>
					  checkTypeEq (
					      t,
					      typeExp,
					      p,
					      "Mismatched types of fields property. Expect: " ^ T.name(t) ^ " . Received: "^ T.name(typeExp)
					  )
					| NONE  => Err.error pos ("Field '" ^ S.name(s) ^ "' is unknown in type " ^ S.name(typ)));
				     checkFields(tl))
				end
			in
			    (
			      if List.length fieldExps <> List.length types
			      then Err.error pos ("RecordExp and record type '" ^ S.name(typ) ^ "' doesn't match")
			      else checkFields fieldExps;
			      {exp = (), ty = T.RECORD (types, refer)}
			    )
			end
			    
		      | SOME _ => (Err.error pos (S.name(typ) ^ " does not have type record"); {exp = (), ty = T.RECORD ([], ref ())})
		      | NONE => (Err.error pos ("type " ^ S.name(typ)  ^ " can't be found"); {exp = (), ty = T.RECORD ([], ref ())})
		end

	    and checkSeqExp (xs) = foldl (fn ((exp, pos), _) => trExp exp) {exp=(), ty=T.UNIT} xs

	    and checkAssignExp ({var, exp, pos}) =
		let
		    val {ty = typeLeft, exp = _} = transVar(vEnv, tEnv, var)
		    val {ty = typeRight, exp = _} = trExp exp
		    val msg = "Can't assign type " ^ T.name(typeRight) ^ " to type " ^ T.name(typeLeft)
		in
		    (checkTypeEq (typeLeft, typeRight, pos, msg); { exp=(), ty=T.UNIT })
		end

	    and checkIfExp ({test = testExp, then' = thenExp, else' = elseOption, pos}) =
		let
		    val typeThenExp = trExp thenExp
		in
		    (checkTypeEq (#ty (trExp testExp), T.INT, pos, "if test clause does not have type int");
		     case elseOption of
		         NONE => typeThenExp
		       | SOME elseExp => (checkTypeEq (#ty typeThenExp, #ty (trExp elseExp), pos, "Mismatched types between then and else");
					  typeThenExp))
		end

	    and checkWhileExp ({test, body, pos}) = (checkTypeEq (#ty (trExp test), T.INT, pos, "while test clause does not have type int");
						     (*checkTypeEq (#ty (trExp body), T.UNIT, pos, "body clause does not have type unit");*)
						     trExp body;
						     { exp = (), ty = T.UNIT })
		   
	    and checkForExp ({escape = _, var, lo, hi, body, pos}) = (checkTypeEq (#ty (trExp lo), T.INT, pos, "from-for clause does not have type int");
						     checkTypeEq (#ty (trExp hi), T.INT, pos, "to-for clause does not have type int");
						     (* checkTypeEq (#ty (trExp body), T.UNIT, pos, "body of for clause does not have type unit"); *)
						     trExp body;
						     { exp = (), ty = T.UNIT })
											   
	    and checkLetExp ({decs, body, pos}) =
		let
		    val helper = fn (dec, {venv = vEnv, tenv = tEnv}) => transDec(vEnv, tEnv, dec)
		    val {venv = vEnv, tenv = tEnv} = foldl helper {venv = vEnv, tenv = tEnv} decs
		in
		    transExp (vEnv, tEnv, body)
		end

	    and checkArrayExp ({typ, size, init, pos}) =
		case S.look(tEnv, typ) of
		    SOME (T.ARRAY(ty, unique)) =>
		    (checkTypeEq (#ty (trExp size), T.INT, pos, "Size of array must have type " ^ T.name(T.INT));
		     checkTypeEq (#ty (trExp init), ty, pos, "Initialize value of array does not have type " ^ T.name(ty));
		     {exp = (), ty = T.ARRAY(ty, unique)})
		  | SOME _ => (Err.error pos (S.name(typ) ^ " does not exist"); {exp = (), ty = T.ARRAY(T.NIL, ref ())})
		  | NONE => (Err.error pos ("Type " ^ S.name(typ) ^ " could not be found"); {exp = (), ty = T.ARRAY(T.NIL, ref ())})
		
		    
							
	    and trExp (A.VarExp(var)) = transVar(vEnv, tEnv, var)
	      | trExp (A.NilExp) = {exp = (), ty = T.NIL}
	      | trExp (A.IntExp _) = {exp = (), ty = T.INT}
	      | trExp (A.StringExp _) = {exp = (), ty = T.STRING}
	      | trExp (A.CallExp e) = checkFnCallExp e
	      | trExp (A.OpExp e) = checkTypeOp e
	      | trExp (A.RecordExp e) = checkRecordExp e
	      | trExp (A.SeqExp e) = checkSeqExp e
	      | trExp (A.AssignExp e) = checkAssignExp e
	      | trExp (A.IfExp e) = checkIfExp e
	      | trExp (A.WhileExp e) = checkWhileExp e
	      | trExp (A.ForExp e) = checkForExp e
	      | trExp (A.BreakExp pos) = {exp = (), ty = T.STRING} (* later *)
	      | trExp (A.LetExp e) = checkLetExp e
	      | trExp (A.ArrayExp e) = checkArrayExp e
	in	    
	    trExp exp
	end

    fun transProg(exp: Absyn.exp): unit = ()

					      
    
end
