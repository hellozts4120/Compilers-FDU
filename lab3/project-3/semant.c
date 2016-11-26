#include "semant.h"

Ty_ty actual_ty(Ty_ty ty){
    while(ty && ty->kind == Ty_name) {
        ty = ty->u.name.ty;
    }
    return ty;
}

Ty_tyList makeFormalTyList(S_table tenv, A_fieldList a_fielfList) {
    // return a list of tenv's all types
    Ty_tyList head = NULL;
	Ty_tyList tail = NULL;

	while(a_fielfList){
        A_field field = a_fielfList->head;
        Ty_ty ty_ty = S_look(tenv, field->typ);
        if (!ty_ty) {
            EM_error(field->pos, "undefined type %s", S_name(field->typ));
            ty_ty = Ty_Int();
        }
        if (!tail) {
            tail = Ty_TyList(ty_ty, NULL);
            head = tail;
        } else {
            tail->tail = Ty_TyList(ty_ty, NULL);
            tail = tail->tail;
        }
        a_fielfList = a_fielfList->tail;
	}
	return head;
}

// assert if the args of expList and tyList are the same
int isSameArgs(S_table venv, S_table tenv, A_expList el, Ty_tyList tl, A_exp fun) {
    struct expty et;
    A_expList eList = el;
    Ty_tyList tList = tl;

    // check args recursively
    while (eList && tList) {
        et = transExp(venv, tenv, eList->head);
        if (tList->head->kind != (et.ty)->kind) {
            // special case, also the same
            if ((actual_ty(tList->head))->kind == (et.ty)->kind || (tList->head->kind == Ty_record && (et.ty)->kind == Ty_nil)) {
                tList = tList->tail;
                eList = eList->tail;
                continue;
            }
            // if just type mismatched, continue execute, as we also need to judge number mismatch
            EM_error(eList->head->pos, "para type mismatched");
        }
        eList = eList->tail;
        tList = tList->tail;
    }
    if (eList && !tList) {  // more args than it should
        string name = S_name(fun->u.call.func);
        EM_error(eList->head->pos - 1 - strlen(name), "para type mismatched");
        return 0;
    } else if (!eList && tList) {  // less args
        EM_error(fun->u.call.args->head->pos, "para type mismatched");
        return 0;
    } else {
        return 1;
    }
}

// assert if two type are the same
int isSameType(Ty_ty first, Ty_ty second) {
    Ty_ty t1 = actual_ty(first);
    Ty_ty t2 = actual_ty(second);
    int kind1 = t1->kind;
    int kind2 = t2->kind;

    return (
        ((kind1 == Ty_record || kind1 == Ty_array) && t1 == t2) ||
        (kind1 == Ty_record && kind2 == Ty_nil) ||
        (kind2 == Ty_record && kind1 == Ty_nil) ||
        (kind1 != Ty_record && kind1 != Ty_array && kind1 == kind2)
    );
}

int isEfieldsMatch(S_table venv, S_table tenv, Ty_ty t, A_exp e) {
    Ty_fieldList tList = t->u.record;
    A_efieldList fList = e->u.record.fields;
    // check type recursively
    while (tList && fList) {
        Ty_field t = tList->head;
        A_efield f = fList->head;
        Ty_ty fTy = transExp(venv, tenv, f->exp).ty;
        if (t->ty->kind != fTy->kind) {
            t->ty = actual_ty(t->ty);
            if (t->ty->kind == Ty_record && fTy->kind == Ty_nil || t->ty->kind == fTy->kind) {
                tList = tList->tail;
                fList = fList->tail;
                continue;
            } else {
                EM_error(e->pos, "para type mismatched");
                return 0;
            }
        }
        tList = tList->tail;
        fList = fList->tail;
    }
    if (tList && !fList) {
        EM_error(e->pos, "para type mismatched");
        return 0;
    } else if (!tList && fList) {
        EM_error(e->pos, "para type mismatched");
        return 0;
    }
    return 1;
}

struct expty expTy(Tr_exp exp, Ty_ty ty) {
    struct expty et;
    et.exp = exp;
    et.ty = ty;
    return et;
}

void SEM_transProg(A_exp exp) {
    struct expty et;
    S_table venv = E_base_venv();
    S_table tenv = E_base_tenv();
    et = transExp(venv, tenv, exp);
}

struct expty transVar(S_table venv, S_table tenv, A_var v) {
    switch (v->kind) {
        case A_simpleVar: {  // var id (a)
            E_enventry x = S_look(venv, v->u.simple);
            // check var type
            if (x && x->kind == E_varEntry) {
                return expTy(NULL, actual_ty(x->u.var.ty));
            } else {
                EM_error(v->pos, "undefined  variable %s", S_name(v->u.simple));
                return expTy(NULL, Ty_Int());
            }
        }
        case A_fieldVar: {  // var record (a.b)
            struct expty firstVar = transVar(venv, tenv, v->u.field.var);
            // check record type
            if (firstVar.ty->kind != Ty_record) {
                EM_error(v->pos, "record var required");
                return expTy(NULL, Ty_Record(NULL));
            } else {
                Ty_fieldList fl;
                // search for field same as record
                for (fl = firstVar.ty->u.record; fl; fl = fl->tail) {
                    if (fl->head->name == v->u.field.sym) {
                        break;
                    }
                }
                if (!fl) {
                    EM_error(v->pos, "field %s doesn't exist", S_name(v->u.field.sym));
                    return expTy(NULL, NULL);
                } else {
                    return expTy(NULL, actual_ty(fl->head->ty));
                }
            }
            return expTy(NULL, Ty_Record(NULL));
        }
        case A_subscriptVar: {  // var array (a[b])
            struct expty var = transVar(venv, tenv, v->u.subscript.var);
            struct expty exp = transExp(venv, tenv, v->u.subscript.exp);
            // check array type
            if (var.ty->kind != Ty_array) {
                EM_error(v->pos, "array required");
                return expTy(NULL, Ty_Array(NULL));
            } else {
                // subscript of the array must be int type
                if (exp.ty->kind != Ty_int) {
                    EM_error(v->pos, "int required");
                    return expTy(NULL, Ty_Int());
                }
                return expTy(NULL, actual_ty(var.ty->u.array));
            }
        }
        default: {
            return expTy(NULL, NULL);
        }
    }
}

struct expty transExp(S_table venv, S_table tenv, A_exp a) {
    switch (a->kind) {
        case A_varExp: {
            return transVar(venv, tenv, a->u.var);
        }
        case A_nilExp: {
            return expTy(NULL, Ty_Nil());
        }
        case A_intExp: {
            return expTy(NULL, Ty_Int());
        }
        case A_stringExp: {
            return expTy(NULL, Ty_String());
        }
        case A_callExp: {
            E_enventry info = S_look(venv, a->u.call.func);
            if (info && info->kind == E_funEntry) {
                // check if args are matched, include type compare and number compare
                if (isSameArgs(venv, tenv, a->u.call.args, info->u.fun.formals, a)) {
                    if (info->u.fun.result) {
                        return expTy(NULL, actual_ty(info->u.fun.result));
                    } else {
                        return expTy(NULL, Ty_Void());
                    }
                } else {
                    return expTy(NULL, Ty_Int());
                }
            } else {
                EM_error(a->pos, "undefined function %s\n", S_name(a->u.call.func));
                return expTy(NULL, Ty_Int());
            }
            return expTy(NULL, Ty_Void());
        }
        case A_opExp: {
            A_oper oper = a->u.op.oper;
            struct expty left = transExp(venv, tenv, a->u.op.left);
            struct expty right = transExp(venv, tenv, a->u.op.right);
            if (oper == A_plusOp || oper == A_minusOp || oper == A_timesOp || oper == A_divideOp) {
                // +-*/, both exp must be int type 
                if (left.ty->kind != Ty_int) {
                    EM_error(a->u.op.left->pos, "integer required");
                }
                if (right.ty->kind != Ty_int) {
                    EM_error(a->u.op.right->pos, "integer required");
                }
            } else if (oper == A_eqOp || oper == A_neqOp || oper == A_leOp || oper == A_ltOp || oper == A_gtOp || oper == A_geOp) {
                // both exp must be same type
                if (oper == A_eqOp || oper == A_neqOp) {  // check record type can be nil
                    if (left.ty->kind == Ty_record && right.ty->kind == Ty_nil) {
                        return expTy(NULL, Ty_Int());
                    }
                    if (left.ty->kind == Ty_nil && right.ty->kind == Ty_record) {
                        return expTy(NULL, Ty_Int());
                    }
                }
                if (left.ty->kind != right.ty->kind) {
                    EM_error(a->pos, "same  type required");	
                }
                return expTy(NULL, Ty_Int());
            }
            return expTy(NULL, Ty_Int());
        }
        case A_recordExp: {
            Ty_ty record = actual_ty(S_look(tenv, a->u.record.typ));
            // type must be record
            if (!record || record->kind != Ty_record) {
                EM_error(a->pos, "undefined record %s", S_name(a->u.record.typ));
                return expTy(NULL, Ty_Int());
            }

            // record field's type must match
            if (isEfieldsMatch(venv, tenv, record, a)) {
                return expTy(NULL, record);
            }
            return expTy(NULL, Ty_Record(NULL));
        }
        case A_seqExp: {
            A_expList eList = a->u.seq;
            // handle eList recursively
            while(eList->tail) {
                transExp(venv, tenv, eList->head);
                eList = eList->tail;
            }
            if (eList && eList->head) {
                return transExp(venv, tenv, eList->head);
            } else {
                return expTy(NULL, Ty_Void());
            }
        }
        case A_assignExp: {
            struct expty var = transVar(venv, tenv, a->u.assign.var);
            struct expty exp = transExp(venv, tenv, a->u.assign.exp);
            
            // var and exp must be the same type
            if (var.ty && !isSameType(var.ty, exp.ty)) {
                EM_error(a->pos, "type mismatch");
            }
            
            return expTy(NULL, Ty_Void());
        }
        case A_ifExp: {
            transExp(venv, tenv, a->u.iff.test);
            struct expty then = transExp(venv, tenv, a->u.iff.then);
            if (a->u.iff.elsee) { // for else-part
                struct expty elsee = transExp(venv, tenv, a->u.iff.elsee);
                // then-exp and else-exp must be the same type
                if((then.ty->kind != elsee.ty->kind) && (then.ty->kind != Ty_nil && elsee.ty->kind != Ty_nil)) {
                    EM_error(a->u.iff.elsee->pos, "then exp and else exp type mismatch");
                }
                return then;
            } else {
                if(then.ty->kind != Ty_void) {
                    EM_error(a->u.iff.then->pos, "this exp must produce no value");
                }
                return expTy(NULL, Ty_Void());
            }
        }
        case A_whileExp: {
            transExp(venv, tenv, a->u.whilee.test);
            struct expty whileBody = transExp(venv, tenv, a->u.whilee.body);

            // prevent dead loop happening
            if(whileBody.ty->kind == Ty_int) {
                EM_error(a->u.whilee.body->pos, "this exp must produce no value");
            }
            return expTy(NULL, Ty_Void());
        }
        case A_forExp: {
            // get lowest and highest index of a for exp
            struct expty lo = transExp(venv, tenv, a->u.forr.lo);
            struct expty hi = transExp(venv, tenv, a->u.forr.hi);
            struct expty body;

            // lowest index and highest index must be int type
            if (lo.ty->kind != Ty_int) {
                EM_error(a->u.forr.lo->pos, "integer type required");
            }
            if (hi.ty->kind != Ty_int) {
                EM_error(a->u.forr.hi->pos, "integer type required");
            }

            S_beginScope(venv);
            transDec(venv, tenv, A_VarDec(a->pos, a->u.forr.var, S_Symbol("int"), a->u.forr.lo));
            body = transExp(venv, tenv, a->u.forr.body);
            S_endScope(venv);
            return expTy(NULL, Ty_Void());
        }
        case A_breakExp: {
            return expTy(NULL, Ty_Void());
        }
        case A_letExp: {
            S_beginScope(venv);
            S_beginScope(tenv);
            
            // check each dec
            A_decList dec = a->u.let.decs;
            while(dec) {
                transDec(venv, tenv, dec->head);
                dec = dec->tail;
            }
            // check dec body
            struct expty et = transExp(venv, tenv, a->u.let.body);
            
            S_endScope(tenv);
            S_endScope(venv);
            return et;
        }
        case A_arrayExp: {
            // exp type must be equal to array
            Ty_ty array = actual_ty(S_look(tenv, a->u.array.typ));
            if (!array || array->kind != Ty_array) {
                EM_error(a->pos, "undefined array type %s", S_name(a->u.array.typ));
                return expTy(NULL, array);
            }
            
            // array's size type must be int
            struct expty sizeType = transExp(venv, tenv, a->u.array.size);
            if (sizeType.ty->kind != Ty_int) {
                EM_error(a->pos, "array size should be int");
            }
            
            // array's init type must be same as array
            struct expty initType = transExp(venv, tenv, a->u.array.init);
            if (initType.ty->kind != actual_ty(array->u.array)->kind) {
                EM_error(a->u.array.init->pos, "type mismatched");
            }
            
            return expTy(NULL, array);
        }
        default: {
            return expTy(NULL, NULL);
        }
    }
}

void transDec (S_table venv, S_table tenv, A_dec d) {
    
}

Ty_ty transTy (S_table tenv, A_ty a) {
    
}