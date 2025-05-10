#include "symbol_table.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>


static Scope *currentScope=NULL;

/* hash func */
static unsigned hash(const char *s){
    unsigned h=0; while(*s) h=(h<<4)+*s++;
    return h%HASH_SIZE;
}

void pushScope(void){
    Scope *s=calloc(1,sizeof(Scope));
    s->parent=currentScope;
    currentScope=s;
}

void popScope(void){
    if(!currentScope) return;
    /* TODO: free symbols */
    Scope *p=currentScope->parent;
    free(currentScope);
    currentScope=p;
}

static Symbol* findIn(Scope *s,const char*name){
    for(Symbol *sym=s->hash[hash(name)]; sym; sym=sym->next)
        if(strcmp(sym->name,name)==0) return sym;
    return NULL;
}

Symbol* lookup(const char *name){
    for(Scope *s=currentScope; s; s=s->parent){
        Symbol *sym=findIn(s,name);
        if(sym) return sym;
    }
    return NULL;
}

Symbol* lookupCurrent(const char *name){
    if(!currentScope) return NULL;
    return findIn(currentScope,name);
}

Symbol* insert(Symbol proto){
    if(lookupCurrent(proto.name)) return NULL;
    Symbol *sym=malloc(sizeof(Symbol));
    *sym=proto;
    unsigned h=hash(proto.name);
    sym->next=currentScope->hash[h];
    currentScope->hash[h]=sym;
    return sym;
}
