
structure Env :> ENV = 
struct
    type access = unit

    datatype enventry = VarEntry of {ty: T.ty, access: Translate.access}
                      | FunEntry of {
			  level: Translate.level,
			  label: Temp.label,
			  formals: T.ty list,
			  result : T.ty}

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
                            ("print", FunEntry ({formals=[T.STRING], result=T.UNIT, level = Translate.outermost, label = Temp.newlabel()})),
                            ("flush", FunEntry ({formals=[], result=T.UNIT, level = Translate.outermost, label = Temp.newlabel()})),
                            ("getchar", FunEntry ({formals=[], result=T.STRING, level = Translate.outermost, label = Temp.newlabel()})),
                            ("ord", FunEntry ({formals=[T.STRING], result=T.INT, level = Translate.outermost, label = Temp.newlabel()})),
                            ("chr", FunEntry ({formals=[T.INT], result=T.STRING, level = Translate.outermost, label = Temp.newlabel()})),
                            ("size", FunEntry ({formals=[T.STRING], result=T.INT, level = Translate.outermost, label = Temp.newlabel()})),
                            ("substring", FunEntry ({formals=[T.STRING, T.INT, T.INT], result=T.STRING, level = Translate.outermost, label = Temp.newlabel()})),
                            ("concat", FunEntry ({formals=[T.STRING, T.STRING], result=T.STRING, level = Translate.outermost, label = Temp.newlabel()})),
                            ("not", FunEntry ({formals=[T.INT], result=T.INT, level = Translate.outermost, label = Temp.newlabel()})),
                            ("exit", FunEntry ({formals=[T.INT], result=T.UNIT, level = Translate.outermost, label = Temp.newlabel()}))
                        ]
        in
            foldr addtotable S.empty toadd
        end
end
