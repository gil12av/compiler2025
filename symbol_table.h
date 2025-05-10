#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H
#include <stddef.h>


#define HASH_SIZE 211

typedef enum {T_INT,T_REAL,T_CHAR,T_BOOL,T_STRING,
              T_INT_PTR,T_REAL_PTR,T_CHAR_PTR,T_VOID,T_INVALID} Type;

typedef enum {K_VAR,K_PARAM,K_FUNC} Kind;

typedef struct ParamInfo {
    Type type;
    char *name;
} ParamInfo;

typedef struct Symbol {
    char  *name;
    Kind   kind;
    Type   type;      /* var‑type או return‑type */
    int    isDefined; /* עבור פונקציות */
    ParamInfo *params;
    int    paramCount;
    int    line;
    struct Symbol *next; /* לקישור ב‑bucket */
} Symbol;

typedef struct Scope {
    Symbol *hash[HASH_SIZE];
    struct Scope *parent;
} Scope;

/* API */
void    pushScope(void);
void    popScope(void);
Symbol* lookup(const char *name);        /* search current+parents */
Symbol* lookupCurrent(const char *name); /* search only current    */
Symbol* insert(Symbol proto);            /* return NULL if dup     */
void printScope(void);                   /* print current scope    */

#endif
