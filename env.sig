structure T = Types
signature ENV = sig
    type access
	     
    datatype enventry = VarEntry of { ty: T.ty, access: Translate.access }
                      | FunEntry of {formals: T.ty list, result: T.ty,  level: Translate.level, label: Temp.label}

    val base_tenv : T.ty Symbol.table
    val base_venv : enventry Symbol.table
end
