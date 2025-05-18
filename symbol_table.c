#include "symbol_table.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>


static Scope *currentScope=NULL;

/* hash func - מעין טבלת גיבוב ליצירת אינדקס עבור סקופ */
static unsigned hash(const char *s){
    unsigned h=0; while(*s) h=(h<<4)+*s++;
    return h%HASH_SIZE;
}

/* יצירת סקופ חדש לפונקציה או לבלוק ודחיפה לראש המחסנית */
void pushScope(void){
    Scope *s=calloc(1,sizeof(Scope));
    s->parent=currentScope;
    currentScope=s;
}

/* יוצא מהסקופ הנוכחי ומשחרר בזיכרון את כל הסמלים מטבלת הסמלים עצמה*/
void popScope(void){
    for (int i = 0; i < HASH_SIZE; ++i)
    {
        Symbol *sym= currentScope->hash[i];
        while (sym)
        {
            Symbol *next = sym->next; 
            free(sym->name); 
            if(sym->params)
                free(sym->params);
            free(sym);
            sym = next;    /* code */
        }
    }
    
    Scope *p = currentScope->parent;
    free( currentScope );
    currentScope = p;
}

/* Find the symbol in spesific Scope. */
static Symbol* findIn(Scope *s,const char*name){
    for(Symbol *sym=s->hash[hash(name)]; sym; sym=sym->next)
        if(strcmp(sym->name,name)==0) return sym;
    return NULL;
}

/* Find the symbol in all Scopes. */
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


// ================================================================= //
// == FOR Convert enum to str and represent it into symbol table  == //
// ================================================================= //

const char* kindToStr(Kind k){
    switch(k)
    {
        case K_VAR: return "VAR";
        case K_PARAM: return "PARAM";
        case K_FUNC: return "FUNC";
        default: return "UNKNOWN_KIND";
    }
}


const char* typeToStr(Type t){
    switch(t)
    {
        case T_INT: return "INT";
        case T_REAL: return "REAL";
        case T_CHAR: return "CHAR";
        case T_BOOL: return "BOOL";
        case T_STRING: return "STRING";
        case T_INT_PTR: return "INT_PTR";
        case T_REAL_PTR: return "REAL_PTR";
        case T_VOID: return "VOID";
        case T_INVALID: return "INVALID";
        default: return "UNKNOWN_TYPE";
    }
}

// ================================================================= //
// ======================= FOR PRINTING ONLY ======================= //
// ================================================================= //

static void printSingleScope(Scope *s, int indent)
{
    if(!s)
        return;

    for(int i = 0; i < HASH_SIZE; ++i) {
        for(Symbol *sym = s->hash[i]; sym; sym = sym->next) {
            printf(" %*s name = %s, kind = %s, type = %s, line = %d\n",
            indent, " ",
            sym->name,
            kindToStr(sym->kind),
            typeToStr(sym->type),
            sym->line );
        }
    }
}

void printScopes(void)
{      
    puts("\n========== SYMBOL_TABLE: ===========");
    int depth = 0;
    for(Scope *s= currentScope; s; s = s->parent, ++depth)
        printSingleScope(s, depth* 4);
    puts("====================================== \n");       
   

}
