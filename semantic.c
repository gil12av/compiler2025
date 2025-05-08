#include "semantic.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

static Symbol *currentFunction=NULL;

void semanticError(const char *fmt,...){
    extern int yylineno;
    va_list ap; va_start(ap,fmt);
    fprintf(stderr,"Semantic error (line %d): ",yylineno);
    vfprintf(stderr,fmt,ap);
    fprintf(stderr,"\n");
    exit(1);
}

void semInit(void){ pushScope(); }
void semFinish(void){
    Symbol *mainSym=lookup("_main_");
    if(!mainSym || mainSym->kind!=K_FUNC)
        semanticError("missing _main_ function");
    if(mainSym->paramCount!=0 || mainSym->type!=T_VOID)
        semanticError("_main_ must be void with no parameters");
}

void semEnterFunction(Symbol* f){ currentFunction=f; pushScope(); }
void semLeaveFunction(void){ popScope(); currentFunction=NULL; }

/* helpers */
int isPointer(Type t){
    return t==T_INT_PTR||t==T_REAL_PTR||t==T_CHAR_PTR;
}
int isNumeric(Type t){ return t==T_INT||t==T_REAL; }

Type resultBinary(int op,Type a,Type b){
    switch(op){
        case '+': case '-': case '*': case '/':
            if(isNumeric(a)&&isNumeric(b))
                return (a==T_REAL||b==T_REAL)?T_REAL:T_INT;
            break;
        case DOUBLE_EQUAL: case NOT_EQUAL:
            if(a==b) return T_BOOL;
            if(isPointer(a)&&isPointer(b)) return T_BOOL;
            break;
        case '<': case '>': case GREATER_EQUAL: case LESS_EQUAL:
            if(isNumeric(a)&&isNumeric(b)) return T_BOOL;
            break;
        case AND: case OR:
            if(a==T_BOOL && b==T_BOOL) return T_BOOL;
            break;
    }
    return T_INVALID;
}

