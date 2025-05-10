#ifndef SEMANTICS_H
#define SEMANTICS_H

#include "y.tab.h"  
#include "symbol_table.h"


/* forward declaration for struct node (מוגדר ב‑parser.y) */
struct node;              
typedef struct node node; /* נוח לקצר */

void  semInit(void);
void  semFinish(void);
void  semEnterFunction(Symbol* f);
void  semLeaveFunction(void);

Type  semTypeOfNode(node *n);   /* (optional) */
int   isPointer(Type t);
int   isNumeric(Type t);

Type  resultBinary(int op,Type a,Type b);
Type  resultUnary(int op,Type a);

void  semanticError(const char *fmt,...);

Type semTypeOfLValue(node *n);
int semCheckAssign(Type lhs, Type rhs);
int semCheckReturn(Type ret);
int semCheckCall(Symbol *f, node *args);

// Check bool type :
int semCheckCondition(Type t);



#endif
