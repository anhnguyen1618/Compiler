structure P = Parse

val DIR = "test/"

fun test fileName = P.parse (DIR ^ fileName ^ ".tig")

val testIf = case test "if" of
                A.IfExp{test = A.IntExp(1), then' = A.IntExp(1), else' = NONE, pos = _} => true
            |   _ => false


val testIfElse = case test "ifElse" of
                A.IfExp{
                    test = A.OpExp{left = A.IntExp(1), right = A.IntExp(2), oper = LeOp, pos = _},
                    then' = A.IntExp(1),
                    else' = SOME (A.IntExp 0),
                    pos = _} => true
                | _ => false

val testWhile = case test "while" of
                A.WhileExp{test = A.IntExp(1), body = A.IntExp(1), pos = _} => true
                | _ => false

val testFor = case test "for" of
                A.ForExp{var, escape = _, lo = A.IntExp(0), hi =A.IntExp(100), body = A.IntExp(1), pos = _} => S.name(var)= "i"
                | _ => false

val testLetAssignMath = case test "let" of
                A.LetExp{
                    decs = [A.VarDec{name = name1, escape = _, typ = SOME (ty1, _), init = A.IntExp(5), pos = _},
                            A.VarDec{name = name2, escape = _, typ = SOME (ty2, _), init = A.IntExp(6), pos = _},
                            A.VarDec{name = name3, escape = _, typ = SOME (ty3, _), init = A.IntExp(7), pos = _}],
                    body = A.SeqExp([(A.AssignExp{
                                        var = A.SimpleVar(s, _), 
                                        exp = A.OpExp{
                                            left = A.VarExp(A.SimpleVar(a, _)),
                                            right = A.VarExp(A.SimpleVar(b, _)),
                                            oper = A.MinusOp, pos = _},
                                        pos = _ }, _)]),
                    pos = _} => name1 = a andalso name2 = b andalso ty1 = ty2 andalso ty1 = ty3 andalso s = name3
                | _ => false

val testBoolAndOr = case test "boolAnd" of 
                A.IfExp({ 
                    test = A.IfExp({ test = A.IntExp(1), then' = A.IntExp(2), else' = SOME(A.IntExp(0)), pos = _}),
                    then' = A.IntExp(1),
                    else' = SOME(A.IntExp(3)), pos = _}) => true
                | _ => false

val testFunc = case test "func" of
                A.LetExp{
                    decs = [A.FunctionDec[_, _]],
                    body = A.SeqExp([(A.CallExp{func = _, args = [A.IntExp(0), A.StringExp("str2", _)], pos = _}, _)]),
                    pos = _
                } => true
		|   _ => false

val testVarNil = test "varNil"

val testRecurType = test "recurType"
                
(*

val testCompare = test "compare"

val testFunCall = test "funcall"

val testSeq = test "sequence"

		 
*)
