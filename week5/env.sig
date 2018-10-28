structure T = Types
signature ENV = sig
    type access
	     
    datatype enventry = VarEntry of { ty: T.ty }
                      | FunEntry of {formals: T.ty list, result: T.ty}

    val base_tenv : T.ty Symbol.table
    val base_venv : enventry Symbol.table
end
