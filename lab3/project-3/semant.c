#include "semant.h"

Ty_ty actual_ty(Ty_ty ty){
    while (ty && ty->kind == Ty_name) {
        ty = ty->u.name.ty;
    }
    return ty;
}

Ty_tyList makeFormalTyList(S_table tenv, A_fieldList a_fieldList) {
    // return a list of tenv's all types
    Ty_tyList head = NULL;
	Ty_tyList tail = NULL;

	while (a_fieldList) {
        A_field field = a_fieldList->head;
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
        a_fieldList = a_fieldList->tail;
	}
	return head;
}

Ty_fieldList makeFieldTyList(S_table tenv, A_fieldList a_fieldList) {
    // return a list of tenv's all fields, for A_recordTy trans
    A_fieldList fList = a_fieldList;
    Ty_ty ty_ty;
    Ty_fieldList tList = NULL;
    Ty_fieldList head;

    while (fList) {
        ty_ty = S_look(tenv, fList->head->typ);
        Ty_field tField = Ty_Field(fList->head->name, ty_ty);
        if (!tList) {
            tList = Ty_FieldList(tField, NULL);
            head = tList;
        } else {
            tList->tail = Ty_FieldList(tField, NULL);
            tList = tList->tail;
        }
        fList = fList->tail;
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
    switch (d->kind) {
        case A_functionDec: {
            A_fundec prevFunc = NULL;
            A_fundecList funcList = d->u.function;

            while (funcList) {
                if (strcmp("", S_name(funcList->head->result)) == 0) {  // case of type void
                    funcList->head->result = S_Symbol("void");
                }

                // two different functions must have different names
                if (prevFunc && !strcmp(S_name(funcList->head->name), S_name(prevFunc->name))) {
                    EM_error(prevFunc->pos, "two functions has same name");
                }

                // check formals' list
                Ty_tyList formalTys = makeFormalTyList(tenv, funcList->head->params);
                S_enter(venv, funcList->head->name, E_FunEntry(formalTys, S_look(tenv, funcList->head->result)));

                prevFunc = funcList->head;
                funcList = funcList->tail;
            }

            funcList = d->u.function;

            while (funcList) {
                Ty_tyList formalTys = makeFormalTyList(tenv, funcList->head->params);
                S_beginScope(venv);

                A_fieldList fList = funcList->head->params;
                Ty_tyList tList = formalTys;
                while (fList) {
                    // store their functions' type in a table
                    S_enter(venv, fList->head->name, E_VarEntry(tList->head));
                    fList = fList->tail;
                    tList = tList->tail;
                }

                Ty_ty returnTy = actual_ty(S_look(tenv, funcList->head->result));
                struct expty et = transExp(venv, tenv, funcList->head->body);

                if (returnTy->kind == Ty_void && et.ty->kind != Ty_void) {
                    // error when function void return value
                    EM_error(funcList->head->body->pos, "procedure returns value");
                }

                S_endScope(venv);
                funcList = funcList->tail;
            }
            break;
        }
        case A_varDec: {
            struct expty et;
            A_exp initExp = d->u.var.init;
            // check if exist init exp
            if (!initExp) {  // no init exp, using void
                et = expTy(NULL, Ty_Void());
                S_enter(venv, d->u.var.var, E_VarEntry(et.ty));
                break;
            }
            // with init exp
            Ty_ty ty_ty = NULL;
            if (d->u.var.typ) {
                ty_ty = S_look(tenv, d->u.var.typ);
            }
            et = transExp(venv, tenv, initExp);
            if (!ty_ty) {
                if(et.ty->kind == Ty_nil) {
                    // nil must be constrained by record type
                    EM_error(d->pos, "type required");
                }
                ty_ty = et.ty;
            }
            et.ty = actual_ty(et.ty);
            // case of type mismatch
            if (actual_ty(ty_ty)->kind != et.ty->kind) {
                // if record or et is nil, it's ok
                if (!(actual_ty(ty_ty)->kind == Ty_record && et.ty->kind == Ty_nil)){
                    EM_error(d->u.var.init->pos, "type mismatch");
                }
            }

            // handle specific exp type
            switch (et.ty->kind) {
                case Ty_record: {
                    // one record can only have one record type, can't be init by other record type
                    string initRecord = S_name(initExp->u.record.typ);  // init record type
                    string origRecord = S_name(d->u.var.typ);  // origin record type
                    // if no record name, it's ok. occur strange bug in my system, it should be void string...
                    // version: Ubuntu-14.04.2-amd64
                    if(origRecord != "" && (int)(origRecord) != 4229463 && strcmp(initRecord, origRecord)){
                        EM_error(d->u.var.init->pos, "type mismatch");
                    };
                    break;
                }
                case Ty_array: {
                    // handle case of recursively type dec and recursively function dec
                    string initRecord = S_name(initExp->u.record.typ);
                    string origRecord = S_name(d->u.var.typ);
                    if (!strcmp(initRecord, origRecord)) {
                        break;
                    }

                    // exist d->typ, check with init exp
                    if (S_look(tenv,d->u.var.typ)) {
                        int isSame = 0;
                        Ty_ty ty = S_look(tenv, d->u.var.init->u.array.typ);
                        while (ty && ty->kind == Ty_name) {
                            string name = S_name(ty->u.name.sym);
                            string varName = S_name(d->u.var.typ);
                            if (strcmp("int", name) && strcmp("string", name) && !strcmp(name, varName)) {
                                isSame = 1;
                                break;
                            }
                            ty = ty->u.name.ty;
                        }

                        if (isSame) {
                            break;
                        }

                        if (et.ty->kind == Ty_record) {
                            // if record type, must be same as init exp
                            Ty_ty initType = S_look(tenv, d->u.var.init->u.array.typ);
                            Ty_ty varType = S_look(tenv, d->u.var.typ);
                            if (strcmp(S_name(varType->u.name.sym), S_name(initType->u.name.sym))) {
                                EM_error(d->u.var.init->pos, "type mismatch");
                            }
                        }
                        else {
                            EM_error(d->pos, "type mismatch");
                        }
                    }
                    break;
                }
                default: {
                    break;
                }
            }
            S_enter(venv, d->u.var.var, E_VarEntry(et.ty));
            break;
        }
        case A_typeDec: {
            
        }
        default: {
            break;
        }
    }
}

Ty_ty transTy (S_table tenv, A_ty a) {
    switch (a->kind) {
        case A_nameTy: {
            return Ty_Name(a->u.name, S_look(tenv, a->u.array));
        }
        case A_recordTy: {
            return Ty_Record(makeFieldTyList(tenv, a->u.record));
        }
        case A_arrayTy: {
            return Ty_Array(S_look(tenv, a->u.array));
        }
        default: {
            assert(0);
        }
    }
}