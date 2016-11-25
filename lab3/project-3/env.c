#include "env.h"

E_enventry E_VarEntry(Ty_ty ty) {
    E_enventry varEntry = checked_malloc(sizeof(*varEntry));
    varEntry->kind = E_varEntry;
    varEntry->u.var.ty = ty;
    return varEntry;
}

E_enventry E_FunEntry(Ty_tyList formals, Ty_ty result) {
    E_enventry funEntry = checked_malloc(sizeof(*funEntry));
    funEntry->kind = E_funEntry;
    funEntry->u.fun.formals = formals;
    funEntry->u.fun.result = result;
    return funEntry;
}

S_table E_base_tenv(void) {
    S_table table = S_empty();
    S_enter(table, S_Symbol("int"), Ty_Int());
    S_enter(table, S_Symbol("string"), Ty_String());
    S_enter(table, S_Symbol("nil"), Ty_Nil());
    S_enter(table, S_Symbol("void"), Ty_Void());
    return table;
}

S_table E_base_venv(void) {
    S_table table = S_empty();
    S_enter(table, S_Symbol("print"), E_FunEntry(Ty_TyList(Ty_String(), NULL), Ty_Void()));
    S_enter(table, S_Symbol("flush"), E_FunEntry(NULL, Ty_Void()));
    S_enter(table, S_Symbol("getchar"), E_FunEntry(NULL, Ty_String()));
    S_enter(table, S_Symbol("ord"), E_FunEntry(Ty_TyList(Ty_String(), NULL), Ty_Int()));
    S_enter(table, S_Symbol("chr"), E_FunEntry(Ty_TyList(Ty_Int(), NULL), Ty_String()));
    S_enter(table, S_Symbol("size"), E_FunEntry(Ty_TyList(Ty_String(), NULL), Ty_Int()));
    S_enter(table, S_Symbol("substring"), E_FunEntry(Ty_TyList(Ty_String(), Ty_TyList(Ty_Int(), Ty_TyList(Ty_Int(), NULL))), Ty_String()));
    S_enter(table, S_Symbol("concat"), E_FunEntry(Ty_TyList(Ty_String(), Ty_TyList(Ty_String(), NULL)), Ty_String()));
    S_enter(table, S_Symbol("not"), E_FunEntry(Ty_TyList(Ty_Int(), NULL), Ty_Int()));
    S_enter(table, S_Symbol("exit"), E_FunEntry(Ty_TyList(Ty_Int(), NULL), Ty_Void()));

    return table;
}

