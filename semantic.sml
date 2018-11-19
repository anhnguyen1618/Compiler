
structure E = Env
		  
structure Semant = struct
    type venv = E.enventry Symbol.table
    type tenv = Types.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}
    type decty =  {venv : venv, tenv: tenv, expList: Translate.exp list}
		     
    val nestedLoopLevel = ref 0
    fun changeNestedLoopLevel(oper) = nestedLoopLevel := oper(!nestedLoopLevel, 1)
    fun increaseNestedLevel () = changeNestedLoopLevel op+
    fun decreaseNestedLevel () = changeNestedLoopLevel op-
    fun getNestedLoopLevel () = !nestedLoopLevel

    fun checkInt ({exp, ty = T.INT}, pos) = ()
      | checkInt ({exp, ty = _ }, pos) = Err.error pos "integer required"

    fun checkTypeEq (firstType, secondType, pos, errormsg) =
	if T.eq(firstType, secondType) then () else (Err.error pos errormsg)

    fun actual_ty tEnv ty =
	let
	    fun h (T.NAME(n, result)) =
		(case !result of
		     SOME t => t
		   | NONE => ( case S.look(tEnv, n) of
				   SOME t =>
				   let val typ = h t
				   in
				       result := SOME(typ);
				       typ
				   end
				 | NONE => T.NIL))
	      | h (e) = e
	in
	    h ty
	end


    fun printCircular circularArrays =
	let
	    fun printHelper (h::[]) = print (h ^ "\n")
	      | printHelper (h::tl) = (print (h ^ " -> "); printHelper tl)
	      | printHelper ([]) = () 
	in
	    if List.length circularArrays > 0 then
		(print "Circular type decs:\n";
		foldl (fn (x, _) => printHelper x) () circularArrays)
	    else ()
	end
	      
    fun checkCircular (xs) =
	let
	 val typeNodes  = map (fn x => (x,ref (#name(x)), ref false, ref false)) xs
	 fun traverse ({name, ty, pos}, startNode, isVisited, isCir) =
	     if !isVisited then (false, []) else
	     ( isVisited := true;
	       case ty of
		   A.NameTy(next, _) =>
		   let
		       val nextNode = List.find (fn ({name, ty, pos}, _, _, _) => S.eq(name, next)) typeNodes
		   in
		       case nextNode of
			   SOME (e, nextStartNode, nextIsVisited, nextIsCir) =>
			   if !nextIsVisited andalso S.eq(!startNode, !nextStartNode)
			   then
			       (nextIsCir := true;
				Err.error (#pos e) ("Circular type declaration: " ^ S.name(#name e));
				(true, [S.name(name), S.name(next)]))
			   else if !nextIsVisited then (false, [])
			   else
			       (nextStartNode := !startNode;
				let
				    val (addNew, cirNodes) = traverse (e, nextStartNode, nextIsVisited, nextIsCir)
				in
				    if List.length cirNodes > 0 andalso addNew
				    then
					(not (!isCir), S.name(name)::cirNodes)
				    else (false, cirNodes)
				end
				    
			       )
			 | NONE => (false, [])

		   end
		 | _ => (false, []))
									   
									   
	 fun h ((e, startNode, isVisited, isCir), cir) =
	     if !isVisited then cir else
	     (let
		 val (_, cirNodes) = traverse (e, startNode, isVisited, isCir)
	     in
		 if List.length cirNodes > 0
		 then cirNodes::cir
		 else cir
	     end)
	in
	    foldl h [] typeNodes
	end
	    
	    

    fun	transTy (tEnv: tenv, ty: A.ty): T.ty =
	(let
	    fun lookUpType (s, p) =
		case S.look(tEnv, s) of
		    SOME t => t
		  | NONE => (Err.error p ("Type " ^ S.name(s) ^ " has not been declared"); T.NIL)
	    fun mapFieldToRecord {name, typ, pos, escape = _} = (name, lookUpType(typ, pos))
		    
	    fun checkRecord fields = T.RECORD (map mapFieldToRecord fields, ref ())

	    fun checkNameTy e = lookUpType e

	    fun checkArrayTy e = T.ARRAY(lookUpType e, ref())
			 
	    fun trTy (A.NameTy e): T.ty = checkNameTy e
	      | trTy (A.RecordTy e): T.ty = checkRecord e
	      | trTy (A.ArrayTy e): T.ty = checkArrayTy e
	in
	    trTy ty
	end)
	    
    and transDec (vEnv: venv, tEnv: tenv, level: Translate.level, exps: Absyn.dec list, break: Temp.label): decty =
	let
	    val actual_ty = (actual_ty tEnv) o #ty
	    fun checkVarDec (vEnv: venv, tEnv: tenv, {name, typ, init, pos, escape = _}, expList) =
		let
		    val {exp = initValueIrExp, ty} = transExp(vEnv, tEnv, level, init, break)
		    fun createAssignStm access = expList @ [Translate.assignStm(Translate.simpleVar(access, level), initValueIrExp)]
		in
		    case typ of
			SOME (s, p) => (case S.look(tEnv, s) of
					   SOME t => if T.eq(t, ty)
						     then
							 let
							     val access = Translate.allocLocal(level)(true)
							     val newVenv = S.enter(vEnv, name,
							       E.VarEntry{ty = ty, access = access})
							 in
							     {venv = newVenv,
							      tenv = tEnv,
							      expList = createAssignStm (access)
							     }
							 end
						     else (Err.error p ("can't assign exp type " ^ T.name(ty) ^ " to type " ^ S.name(s));
							   {venv = vEnv, tenv = tEnv, expList = expList})
					 | NONE => (Err.error pos ("Type " ^ S.name(s) ^ " has not been declared");
						    {venv = vEnv, tenv = tEnv, expList = expList}))
		      | NONE => ((if T.eq(ty, T.NIL)
				  then (Err.error pos ("Can't assign Nil to non-record type variable"))
				  else ());
				 {
				   venv = S.enter(vEnv, name, E.VarEntry{ty = ty, access = Translate.allocLocal(level)(true)}),
				   tenv = tEnv,
				   expList = expList
				})
		end

	    fun checkTypeDec (vEnv: venv, tEnv: tenv, xs, expList) =
		let
		    fun addDumbType ({name, ty = _, pos = _}, tEnv) = S.enter(tEnv, name, T.NAME(name, ref NONE))
		    val dumbTenv = foldl addDumbType tEnv xs
		    fun f ({name, ty, pos}, {venv, tenv, expList}) = {tenv = S.enter(tenv, name, transTy(tenv, ty)), venv = venv, expList = expList}
		in
		    (printCircular o checkCircular) xs;
		    foldl f {venv = vEnv, tenv = dumbTenv, expList = expList} xs
		end

	    fun checkFuncDec (vEnv: venv, tEnv: tenv, fs, expList) =
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
			    val typeList = map (actual_ty o getType) params
			    val resultType = getTypeForResult result
			    (* change escape *)
			    val escapes = map (fn x => true) params
			    val label = Temp.newlabel()
			    val newLevel = Translate.newLevel{parent = level, name = label, formals = escapes}
			in
			    S.enter(acc, name, E.FunEntry{formals = typeList, result = resultType, label = label, level = newLevel})
			end
						
		    fun addNewFuncEntry ({name, params, result, body, pos}, curVenv) = 
																   
			let
			    val typeList = map getType params
			    val resultType = getTypeForResult result
			    val funcLevel = case S.look(curVenv, name) of
						SOME(E.FunEntry e) => #level e
					      | _ => Translate.newLevel {parent=level, name=Temp.newlabel(), formals=[]}

			    val paramAccesses = Translate.formals funcLevel
							      
			    val addParamsToBody = fn ({name, ty}, (temp, i)) =>
						     (S.enter(temp, name,
							 E.VarEntry({ ty = ty,
								      access = List.nth(paramAccesses, i)})), i + 1)

			    val (bodyVenv, _) = foldl addParamsToBody (curVenv, 0) typeList
			    val {exp = _, ty = tyBody } = transExp(bodyVenv, tEnv, funcLevel, body, break)
			in
			    (if checkResultType(resultType, tyBody) then ()
			     else (Err.error pos ("return type " ^ T.name(tyBody) ^ " does not match with " ^ T.name(resultType)));
			    curVenv)
			end			    

		    val baseEnv = foldl addFuncHeaders vEnv fs
			    	
		    val newvEnv = foldl addNewFuncEntry baseEnv fs
		in
		    {venv = newvEnv, tenv = tEnv, expList = expList}
		end
		    
	    fun trDec (vEnv, tEnv, A.VarDec(e), expList) = checkVarDec (vEnv, tEnv, e, expList)
	      | trDec (vEnv, tEnv, A.TypeDec(e), expList) = checkTypeDec (vEnv, tEnv, e, expList)
	      | trDec (vEnv, tEnv, A.FunctionDec(e), expList) = checkFuncDec (vEnv, tEnv, e, expList)
						       
	    val helper = fn (dec, {venv = vEnv, tenv = tEnv, expList}) => trDec(vEnv, tEnv, dec, expList)
	    val result = foldl helper {venv = vEnv, tenv = tEnv, expList = []} exps	
	in
	    result
	end
			       
    and transVar (vEnv: venv, tEnv: tenv, level: Translate.level, exp: Absyn.var, break: Temp.label): expty =
	let
	    val actual_ty = actual_ty tEnv
	    val actual_ty_exp = actual_ty o #ty
		
	    fun checkSimpleVar (s, pos) =
		case S.look(vEnv, s) of
		    SOME(E.VarEntry({ty, access})) => {exp = Translate.simpleVar(access, level), ty = actual_ty ty}
		  | SOME _ => {exp = Translate.intExp(0), ty = T.NIL}
		  | NONE => (Err.error pos ("valuable '" ^ S.name(s) ^"' has not been declared");
			     {exp = Translate.intExp(0), ty = T.NIL})

	    and checkFieldVar (obj, s, pos) =
		let
		    val {exp = recExp, ty = ty } = trVar obj
		    val typeOfObj = actual_ty ty
		in
		    case typeOfObj of
			T.RECORD (tys, _) =>
			let
			    val index = ref (~1)
						 
			    val matchedField = List.find (fn (symbol, _) => (index := !index + 1; S.eq(s, symbol))) tys
			in
			    case matchedField of
				SOME (_, ty) => {exp = Translate.fieldVar(recExp, !index), ty = ty}
			      | NONE => (Err.error pos ("Property '"
							^ S.name(s) ^ "' does not exist on type '"
							^ T.name(typeOfObj) ^ "'");
					 {exp = Translate.intExp(0), ty = T.NIL})
			end
		      | _ => (Err.error pos ("Can't access property '"
					     ^ S.name(s) ^ "' of type '"
					     ^ T.name(typeOfObj) ^ "'");
			      {exp = Translate.intExp(0), ty = T.NIL})
		end

	    and checkArrayVar (var, sizeExp, pos) =
		let
		    val {exp = arrayExp, ty = ty} = trVar var
		    val typeOfArr = actual_ty ty
		in
		    case typeOfArr of
			T.ARRAY (ty, _) =>
			let
			    val {exp = sizeIrExp, ty = sizety} = transExp(vEnv, tEnv, level, sizeExp, break)
									 
			in
			    if T.eq(sizety, T.INT)
			    then {exp = Translate.subscriptVar(arrayExp, sizeIrExp), ty = ty}
			    else (Err.error pos "index of array is not int"; {exp = Translate.intExp(0), ty = T.NIL})
			end
		      | _ => (Err.error pos ("Can't access member of non-array type"); {exp = Translate.intExp(0), ty = T.NIL})		    
		end
		    
		    
	    and trVar (A.SimpleVar e) = checkSimpleVar e
	      | trVar (A.FieldVar e) = checkFieldVar e
	      | trVar (A.SubscriptVar e) = checkArrayVar e
	in
	    trVar exp
	end


    and transExp (vEnv: venv, tEnv: tenv, level: Translate.level, exp: Absyn.exp, break: Temp.label): expty =
	let
	    val actual_ty = actual_ty tEnv
	    val actual_ty_exp = actual_ty o #ty
				      
	    fun checkTypeOp ({left, oper, right, pos}) =
		let
		    val (leftResult, rightResult) = (trExp left, trExp right)
		    val {exp = leftIr, ty = leftTy} = leftResult
		    val {exp = rightIr, ty = rightTy} = rightResult
		in
		    checkTypeEq (actual_ty leftTy, T.INT, pos, "Interger is require");
		    checkTypeEq (actual_ty rightTy, T.INT, pos, "Interger is require");
		    {exp = Translate.opExp(leftIr, oper, rightIr), ty = T.INT}
		end

	    and checkFnCallExp ({func, args, pos}) =
		let
		    fun checkParam ((exp, ty), acc): Translate.exp list =
			let
			    val {exp = argIr, ty = argTy } = trExp exp
			    val actualArgType = actual_ty argTy
			in
			    (checkTypeEq (
				ty,
				actualArgType,
				pos,
				"Mismatched types of function args. Expect: "
				^ T.name(ty) ^ " . Received: "
				^ T.name(actualArgType)
			      ); argIr :: acc)
			end
		in
		    case S.look(vEnv, func) of
			SOME (E.FunEntry({formals, result, label, level = decLevel})) =>
			let
			    val argIrs = foldr checkParam [] (ListPair.zip(args, formals))
			in
			    {exp = Translate.funCallExp(decLevel, level, label, argIrs), ty = result}
			end

		      | SOME _ => (Err.error pos (S.name(func) ^ " does not have type function");
				   {exp = Translate.nilExp(), ty = T.UNIT})
		      | NONE => (Err.error pos ("Function " ^ S.name(func) ^ " can't be found");
				 {exp = Translate.nilExp(), ty = T.UNIT})
		end

	    and checkRecordExp {fields, typ, pos} =
		let
		    val preFields = map (fn (symbol, exp, pos) => (symbol, trExp(exp), pos)) fields
		    val fieldValsIR = map (fn (_, {exp, ty =_}, _) => exp) preFields
		    val fieldExps = map (fn (symbol, exp, pos) => (symbol, actual_ty_exp exp, pos)) preFields
		in
		    case S.look(tEnv, typ) of
			SOME (T.RECORD (types, refer)) =>
			let
			    fun checkFields [] = []
			      | checkFields ((s, t, p)::tl) =
				let
				    val matchedField = List.find (fn (symbol, _) => S.eq(s, symbol)) types
				in
				    case matchedField of
					  SOME(symbol, typeExp) =>
					  (checkTypeEq (
					      t,
					      actual_ty(typeExp),
					      p,
					      "Mismatched types of fields property: '"
					      ^S.name(s)^"'. Expect: " ^ T.name(typeExp)
					      ^ " . Received: "^ T.name(t)
					    ); (symbol,typeExp)::checkFields(tl))
					| NONE  =>
					  (
					    Err.error pos ("Field '" ^ S.name(s) ^ "' is unknown in type " ^ S.name(typ));
					    checkFields(tl)
					  )
				end
			in
			    (
			      if List.length fieldExps <> List.length types
			      then (Err.error pos ("RecordExp and record type '" ^ S.name(typ) ^ "' doesn't match");
				    {exp = Translate.recordDec(fieldValsIR), ty = T.RECORD (types, refer)})
			      else
				  let
				      val typesInCreateOrder = checkFields fieldExps;
				  in
				      {exp = Translate.recordDec(fieldValsIR), ty = T.RECORD (typesInCreateOrder, refer)}
				  end
			    )
			end
			    
		      | SOME _ => (Err.error pos (S.name(typ) ^ " does not have type record");
				   {exp = Translate.intExp(0), ty = T.RECORD ([], ref ())})
		      | NONE => (Err.error pos ("type " ^ S.name(typ)  ^ " can't be found");
				 {exp = Translate.intExp(0), ty = T.RECORD ([], ref ())})
		end

	    (*TODO: Handle SEQIR correctly here*)
	    and checkSeqExp (xs) = foldl (fn ((exp, pos), _) => trExp exp) {exp=Translate.nilExp(), ty=T.UNIT} xs

	    and checkAssignExp ({var, exp, pos}) =
		let
		    val {ty = typeLeft, exp = leftExp} = transVar(vEnv, tEnv, level, var, break)
		    val {ty = typeRight, exp = rightExp} = trExp exp
		    val msg = "Can't assign type " ^ T.name(typeRight) ^ " to type " ^ T.name(typeLeft)
		in
		    (checkTypeEq (typeLeft, typeRight, pos, msg); {exp=Translate.assignStm(leftExp, rightExp), ty=T.UNIT })
		end

	    and checkIfExp ({test = testExp, then' = thenExp, else' = elseOption, pos}) =
		let
		    val {exp = testIr, ty = testTy} = trExp testExp
		    val {exp = thenIr, ty = thenTy} = trExp thenExp
		in
		    (checkTypeEq (actual_ty testTy, T.INT, pos, "if test clause does not have type int");
		     case elseOption of
		         NONE => {exp = Translate.ifExp(testIr, thenIr, NONE), ty= thenTy}
		       | SOME elseExp =>
			 let
			     val {exp = elseIr, ty = elseTy} = trExp elseExp
			 in
			     (checkTypeEq (actual_ty thenTy,
					   actual_ty elseTy,
					   pos,
					   "Mismatched types between then and else");
			      {exp = Translate.ifExp(testIr, thenIr, SOME(elseIr)), ty = elseTy})
			 end)
		end

	    and checkWhileExp ({test, body, pos}) =
		let
		    val _ = increaseNestedLevel()
		    val {exp = testExp, ty = testTy} = trExp test
		    val doneLabel = Temp.newlabel()
		    val {exp = bodyExp, ty = _} = transExp (vEnv, tEnv, level, body, doneLabel)
		    val _ = decreaseNestedLevel()

		in
		    checkTypeEq (actual_ty testTy, T.INT, pos, "while test clause does not have type int");		      
		    { exp = Translate.whileExp(testExp, bodyExp, doneLabel), ty = T.UNIT }
		end
		    

							
	    and checkForExp (e) =
		let
		    val whileAST = A.rewriteForExp(e)
		    (* val loTy = (actual_ty_exp o trExp o #lo) e
		    val hiTy = (actual_ty_exp o trExp o #hi) e
		    val _ = checkTypeEq (loTy, T.INT, pos, "from-for clause does not have type int")
		    val _ = checkTypeEq (hiTy, T.INT, pos, "to-for clause does not have type int") *)
		in
		    trExp whileAST
		end		    
		    
	    and checkLetExp ({decs, body, pos}) =
		let
		    val {venv = vEnv, tenv = tEnv, expList} = transDec(vEnv, tEnv, level, decs, break)
		in
		    transExp (vEnv, tEnv, level, body, break)
		end

	    and checkArrayExp ({typ, size, init, pos}) =
		case S.look(tEnv, typ) of
		    SOME (T.ARRAY(ty, unique)) =>
		    let
			val sizeResult = trExp size
			val initResult = trExp init
		    in
			(checkTypeEq (actual_ty_exp sizeResult, T.INT, pos, "Size of array must have type " ^ T.name(T.INT));
			 checkTypeEq (actual_ty_exp initResult, ty, pos, "Initialize value of array does not have type " ^ T.name(ty));
			 {exp = Translate.arrayDec(#exp sizeResult, #exp initResult), ty = T.ARRAY(ty, unique)})
		    end
			
		  | SOME _ => (Err.error pos (S.name(typ) ^ " does not exist"); {exp = Translate.unitExp(), ty = T.ARRAY(T.NIL, ref ())})
		  | NONE => (Err.error pos ("Type " ^ S.name(typ) ^ " could not be found"); {exp = Translate.unitExp(), ty = T.ARRAY(T.NIL, ref ())})
		
		    
							
	    and trExp (A.VarExp(var)) = transVar(vEnv, tEnv, level, var, break)
	      | trExp (A.NilExp) = {exp = Translate.nilExp(), ty = T.NIL}
	      | trExp (A.IntExp e) = {exp = Translate.intExp(e), ty = T.INT}
	      | trExp (A.StringExp (str, _)) = {exp = Translate.stringExp(str), ty = T.STRING}
	      | trExp (A.CallExp e) = checkFnCallExp e
	      | trExp (A.OpExp e) = checkTypeOp e
	      | trExp (A.RecordExp e) = checkRecordExp e
	      | trExp (A.SeqExp e) = checkSeqExp e
	      | trExp (A.AssignExp e) = checkAssignExp e
	      | trExp (A.IfExp e) = checkIfExp e
	      | trExp (A.WhileExp e) = checkWhileExp e
	      | trExp (A.ForExp e) = checkForExp e
	      | trExp (A.BreakExp pos) = (if getNestedLoopLevel() > 0 then () else Err.error pos "Break exp is not nested inside loop";
					  {exp = Translate.breakExp(break), ty = T.STRING})
	      | trExp (A.LetExp e) = checkLetExp e
	      | trExp (A.ArrayExp e) = checkArrayExp e
	in	    
	    trExp exp
	end

    fun transProg (my_exp : A.exp) = 
	let
	    val mainlabel = Temp.newlabel()
	    val _ = FindEscape.findEscape my_exp
	    (*val mainexp = #exp Semant.transExp(Env.base_venv, Env.base_tenv, Translate.outermost, absyn) *)
	in
	    Translate.getResult()
	end
					      
    
end
