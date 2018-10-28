structure A = Absyn
structure Err = ErrorMsg
structure S = Symbol

structure Env :> ENV = 
struct
    type access = unit

    datatype enventry = VarEntry of {ty: T.ty}
                      | FunEntry of {formals: T.ty list, result : T.ty}

    val base_tenv = (* predefined types *)
        let
            fun addtotable ((s, t), table) = S.enter(table, S.symbol s, t)
            val toadd = [("int", T.INT), ("string", T.STRING)]
        in
            foldr addtotable S.empty toadd
        end

    val base_venv = (* predefined functions *)
        let
            fun addtotable ((s, t), table) = S.enter(table, S.symbol s, t)
            val toadd = [
                            ("print", FunEntry ({formals=[T.STRING], result=T.UNIT})),
                            ("flush", FunEntry ({formals=[], result=T.UNIT})),
                            ("getchar", FunEntry ({formals=[], result=T.STRING})),
                            ("ord", FunEntry ({formals=[T.STRING], result=T.INT})),
                            ("chr", FunEntry ({formals=[T.INT], result=T.STRING})),
                            ("size", FunEntry ({formals=[T.STRING], result=T.INT})),
                            ("substring", FunEntry ({formals=[T.STRING, T.INT, T.INT], result=T.STRING})),
                            ("concat", FunEntry ({formals=[T.STRING, T.STRING], result=T.STRING})),
                            ("not", FunEntry ({formals=[T.INT], result=T.INT})),
                            ("exit", FunEntry ({formals=[T.INT], result=T.UNIT}))
                        ]
        in
            foldr addtotable S.empty toadd
        end
end
