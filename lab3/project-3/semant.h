#ifndef SEMANT_H
#define SEMANT_H

#include "env.h"
typedef void *Tr_exp;

struct expty {Tr_exp exp; Ty_ty ty; } ;

struct expty expTy(Tr_exp exp, Ty_ty ty);

void SEM_transProg(A_exp exp);
struct expty transVar (S_table venv, S_table tenv, A_var v);
struct expty transExp (S_table venv, S_table tenv, A_exp a);
void         transDec (S_table venv, S_table tenv, A_dec d);
       Ty_ty transTy  (		     S_table tenv, A_ty a);
Ty_ty actual_ty(Ty_ty ty);
Ty_tyList makeFormalTyList(S_table tenv, A_fieldList a_fieldList);
Ty_fieldList makeFieldTyList(S_table tenv, A_fieldList a_fieldList);

int isSameArgs(S_table venv, S_table tenv, A_expList el, Ty_tyList tl, A_exp fun); 
int isSameType(Ty_ty first, Ty_ty second);
int isEfieldsMatch(S_table venv, S_table tenv, Ty_ty t, A_exp e);
string forIndexVar;

#endif
