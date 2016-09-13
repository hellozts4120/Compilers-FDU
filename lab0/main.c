#include <stdio.h>

#include "util.h"
#include "slp.h"
#include "prog1.h"


int maxargs(A_stm prog){
    switch (prog->kind) {
        case A_compoundStm:
            int stm1Num = maxargs(prog->u.compound.stm1);
            int stm2Num = maxargs(prog->u.compound.stm2);
            return (stm1Num >= stm2Num) ? stm1Num : stm2Num;
        case A_assignStm:
            A_exp exp = prog->u.assign.exp;
            return (exp->kind == A_eseqExp) ? maxargs(exp->u.eseq.stm) : 0;
        case A_printStm:
            A_expList expList = prog->u.print.exps;
            int argNum = 1;
            while (expList->kind != A_lastExpList) {
                ++argNum;
                expList = expList->u.pair.tail;
            }
            return argNum;
        default:
            return 0;
    }
	return 0;
}

typedef struct table *Table_;
struct table {string id; int value; Table_ tail;};
Table_ Table(string id, int value, struct table *tail) {
    Table_ t = checked_malloc(sizeof(*t));
    t->id = id;
    t->value = value;
    t->tail = tail;
    return t;
}
Table_ interpStm(A_stm, Table_);

typedef struct table *intTable_;
struct IntAndTable {int i; Table_ t};
intTable_ intTable(int i; Table_ t) {
    intTable_ it = checked_malloc(sizeof(*it));
    it->value = i;
    it->t = t;
    return it;
}
intTable_ interpExp(A_exp, Table_);

Table_ interpExpList(A_expList, Table_);

int lookup(Table_ t, string key) {
    while (t != NULL && t->id != key) {
        t = t->tail;
    }
    if (t != NULL) {
        return t->value;
    } else {
        return -1;
    }
}

Table_ update(Table_ t, string key, int value) {
    return Table(key, value, t);
}

Table_ interpStm(A_stm s, Table_ t) {
    switch (s->kind) {
        case A_compoundStm:
            return interpStm(s->u.compound.stm2, interpStm(s->u.compound.stm1, t));
        case A_assignStm:
            intTable_ itable = interpExp(s->u.assign.exp, t);
            return update(itable->t, s->u.assign.id, itable->value);
        case A_printStm:
            printf("\n");
            return interpExpList(s->u.print.exps, t);
        default:
            return t;
    }
}

intTable_ interpExp(A_exp exp, Table_ t) {
    switch (exp->kind) {
        case A_IdExp:
            int value = lookup(t, exp->u.id);
            if (!value) {
                printf("[Error] Identifier %s does not exist!\n", exp->u.id);
            } else {
                return intTable(value, t);
            }
        case A_NumExp:
            return intTable(exp->u.num, t);
        case A_OpExp:
            intTable_ leftExp = interpExp(exp->u.op.left, t);
            intTable_ rightExp = interpExp(exp->u.op.right, t);
            switch (exp->u.op.oper) {
                case A_plus:
                    return intTable(leftExp->value + rightExp->value, t);
                case A_minus:
                    return intTable(leftExp->value - rightExp->value, t);
                case A_times:
                    return intTable(leftExp->value * rightExp->value, t);
                case A_div:
                    return intTable(leftExp->value / rightExp->value, t);
                default:
                    return NULL;
            }
        case A_EseqExp:
            return interpExp(exp->u.eseq.exp, interpStm(exp->u.eseq.stm, t));
        default:
            return NULL;
    }
}

Table_ interpExpList(A_expList list, Table_ t) {
    switch (list->kind) {
        case A_PairExpList:
            intTable_ itable = interpExp(list->u.pair.head, t);
            printf("%d ", itable->value);
            return interpExpList(list->u.pair.tail, itable->t);
        case A_lastExpList:
            intTable_ itable = interpExp(list->u.last, t);
            printf("%d ", itable->value);
            return itable->t;
        default:
            return t;
    }
}

void interp(A_stm prog){
	interpStm(prog);
}

// DO NOT CHANGE!
int main(){
	printf(">> Right Prog Section:\n");
	A_stm rp = right_prog();
	printf("the maximum number of arguments of any print statement is %d\n",maxargs(rp));
	interp(rp);

	printf(">> Error Prog Section:\n");
	A_stm ep = error_prog();
	printf("the maximum number of arguments of any print statement is %d\n",maxargs(ep));
	interp(ep);	
	return 0;
}

