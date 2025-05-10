#include "semantic.h"
#include "symbol_table.h"
#include "y.tab.h"

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

void semInit(void)
{ 
    pushScope(); 
}


void semFinish(void)
{
    Symbol *mainSym=lookup("_main_");
    if(!mainSym || mainSym->kind!=K_FUNC)
        semanticError("missing _main_ function");
    if(mainSym->paramCount!=0 || mainSym->type!=T_VOID)
        semanticError("_main_ must be void with no parameters");
}


int semCheckAssign(Type lhs, Type rhs) 
{
    if(lhs == rhs) return 1;
    if(isNumeric(lhs) && isNumeric(rhs)) return 1;
    return 0;
}

Type semTypeOfLValue(node *n) 
{
    return semTypeOfNode(n);
}

int semCheckReturn(Type ret) 
{
    if(!currentFunction) return 0;
    return(currentFunction->type == ret);
}

int semCheckCall(Symbol *f, node *args){
    (void)f; (void)args;
    return 1;
}


void semEnterFunction(Symbol* f)
{ 
    currentFunction=f; 
    pushScope(); 
}

void semLeaveFunction(void)
{
    popScope();
    currentFunction=NULL; 
}

/* helpers */

Type semTypeOfNode(node* n)
{
    if(!n)
        return T_INVALID;
        return n->type;
}


int isPointer(Type t)
{
    return t==T_INT_PTR||t==T_REAL_PTR||t==T_CHAR_PTR;
}

int isNumeric(Type t)
{
     return t==T_INT||t==T_REAL; 
}

int semCheckCondition(Type t)
{
    return ( t == T_BOOL);
}

Type resultBinary(int op,Type a,Type b)
{
    switch(op){
        case '+': case '-': case '*': case '/':
            if( isNumeric(a) && isNumeric(b) )
                return ( a == T_REAL || b == T_REAL )?T_REAL:T_INT;
            break;
        
        case DOUBLE_EQUAL: case NOT_EQUAL:
            if(a==b)                            return T_BOOL;
            if(isPointer(a)&&isPointer(b))      return T_BOOL;
            break;

        case '<': case '>': case GREATER_EQUAL: case LESS_EQUAL:
            if( isNumeric(a) && isNumeric(b) )  return T_BOOL;
            break;

        case AND: case OR:
            if( a == T_BOOL && b == T_BOOL )    return T_BOOL;
            break;
    }
    return T_INVALID;
}

Type resultUnary(int op,Type a){
    switch(op){
        case NOT: 
            if(a == T_BOOL)       return T_BOOL;
            break;

        case '|':
            if(isNumeric(a))       return a;
            if(a == T_STRING)      return T_INT;
            break;

        case '*': 
            if(a == T_INT_PTR)     return T_INT;
            if(a == T_REAL_PTR)    return T_REAL;
            if(a == T_CHAR_PTR)    return T_CHAR;
            break;

        case '&':
            if(a == T_INT)     return T_INT_PTR;
            if(a == T_REAL)    return T_REAL_PTR;
            if(a == T_CHAR)    return T_CHAR_PTR;
            break;
    }
    return T_INVALID;



}


