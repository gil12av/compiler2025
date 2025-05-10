%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int yylex(void);
int yyerror(char *s);
int yydebug = 1;
extern int yylineno; 
extern char *yytext;

#include "symbol_table.h"
#include "semantic.h"


typedef struct node {
    char *token;
    struct node *left;
    struct node *right;
    Type type;
} node;

node* mknode(char *token, node *left, node *right);
void printtree(node* tree, int level);

// global root this root node is the "start" node .
node* root = NULL;

%}

/*the union tell yacc about the types */

%union {
    char *string;
    struct node* node;
}

/*the left , right is the key words are for the order of the expression */

%left '+' '-'
%left '*' '/'
%left '<' '>' DOUBLE_EQUAL NOT_EQUAL GREATER_EQUAL LESS_EQUAL

%left OR
%left AND
%right NOT


%token <string> BOOL CHAR INT REAL STRING COMMENT
%token <string> INT_PTR CHAR_PTR REAL_PTR
%token <string> IF ELIF ELSE WHILE FOR
%token <string> TYPE VAR PAR RETURN_KEYWORD NULL_KEYWORD DO RETURNS
%token <string> BEGIN_KEYWORD END_KEYWORD DEF CALL
%token <string> TRUE_LITERAL FALSE_LITERAL
%token <string> HEX_LITERAL INT_LITERAL REAL_LITERAL STRING_LITERAL CHAR_LITERAL
%token <string> IDENTIFIER
%nonassoc COMMENT

%type <node> program function_list function return_value parameter_list parameter Comments
%type <node> type declaration_list declaration variable_list variable1
%type <node> literal code_block inner_block optional_function_list optional_var
%type <node> statement_list statement lvalue simple_statement return_statement
%type <node> call_statement if_statement elif_list elif_blocks elif_block else_block
%type <node> single_block_or_block while_statement do_while_statement for_statement
%type <node> expression simple_expression expression_list pointer_expression call_expression



%start program

%%
/* =============== PROGRAM =================*/
program: function_list  
        { 
            semInit();
           
            $$ = mknode("CODE", $1, NULL); 
            root = $$;
            semFinish();
        } 
        ;
/*
            program
                |
            function_list
*/

/* -------------- FUNCTION_LIST ------------------ */
function_list:  function                    { $$ = $1; }
              | function_list function      
                { 
                    node* temp = $1;
                    while(temp->right != NULL)
                        temp = temp->right;
                    temp->right = $2;    
                    $$ = $1; 
                }
              ;

/*
option 1 :
            function_list
                |
            function
option 2 :
            function_list
                /   \
        function_list function


note : the while loop containing the "functions" for us to control the tree
*/
/* -------------- FUNCTION ------------------ */
function: DEF IDENTIFIER '(' parameter_list ')' ':' return_value code_block 
            {
                //-- symbol_table - Part2 : --//
               
                semEnterFunction(lookup($2)); // we want to enter to the scope.

                Symbol proto = { 
                    .name = $2,
                    .kind = K_FUNC, 
                    .type = $7->type,
                    .isDefined = 1,
                    .params = NULL,
                    .paramCount = 0, 
                    .line = yylineno
                    } ;

                if(!insert(proto)) 
                    semanticError("Function %s redecleared", $2);
                semEnterFunction(lookup($2));                

                // AST NODE-build : //
                node* nameNode = mknode($2, NULL, NULL);    // Function name
                node* parsNode = $4;                    // Pararmeter
                node* retNode = $7;                     // Return_value
                node* bodyNode = mknode("BODY", $8, NULL);  // Code_block 

                node* header = mknode("", nameNode, parsNode);
                node* parts = mknode("", retNode, bodyNode);
                $$ = mknode("FUNC",header, parts);

                // Exit Scope: (Semantic)
                semLeaveFunction();
            } 
            ;

return_value :  /*empty*/       
                    { 
                        $$ = mknode("RET NONE", NULL, NULL); 
                        $$->type = T_VOID; // where no return-Value
                    }

               | RETURNS type  
                { 
                    char returnValue[256];
                    sprintf(returnValue, "RET %s", $2->token);
                    $$ = mknode(returnValue, NULL, NULL);
                    $$->type = $2->type; // keep the return type !
                }    
               ;

// about ast: if there just 1 parameter, we return PARS and if theres more than 1 we create node PARS
parameter_list: /*empty*/                       { $$ = mknode("PARS NONE", NULL, NULL); }                      
                | parameter                     { $$ = mknode("PARS", $1, NULL); } 
                | parameter_list ';' parameter  
                   { 
                       node* temp = $1;
                       while(temp->right != NULL) 
                          temp = temp->right;
                       temp->right = $3;
                       $$ = $1;   
                   }  
                ;

// about ast: we want to print example: (par1 INT x) --> so we use buffer.
parameter: PAR type ':' IDENTIFIER  
           { 
                char buffer[256];
                sprintf(buffer, "%s %s %s", $1, $2->token, $4);
                $$ = mknode(buffer, NULL, NULL);

                //-- symbol_table - Part2 : --//
                Symbol proto = { 
                    .name = $4,
                    .kind = K_PARAM, 
                    .type = $2->type,
                    .isDefined = 1,
                    .params = NULL,
                    .paramCount = 0, 
                    .line = yylineno
                    } ;

                if(!insert(proto)) 
                    semanticError("Parameter %s redecleared", $4);
           }
           ;

// about ast: each of types are return from scanner to PAR or return_value.
type:  INT          { $$ = mknode("INT", NULL,NULL);      $$->type = T_INT; }
     | REAL         { $$ = mknode("REAL", NULL,NULL);     $$->type = T_REAL; }
     | CHAR         { $$ = mknode("CHAR", NULL,NULL);     $$->type = T_CHAR; }
     | STRING       { $$ = mknode("STRING", NULL,NULL);   $$->type = T_STRING; }
     | BOOL         { $$ = mknode("BOOL", NULL,NULL);     $$->type = T_BOOL; }
     | INT_PTR      { $$ = mknode("INT_PTR", NULL,NULL);  $$->type = T_INT_PTR; }
     | CHAR_PTR     { $$ = mknode("CHAR_PTR", NULL,NULL); $$->type = T_CHAR_PTR; }
     | REAL_PTR     { $$ = mknode("REAL_PTR", NULL,NULL); $$->type = T_REAL_PTR; }
     ;
Comments: /*empty*/ { $$ = NULL; } 
        | COMMENT { $$ = NULL; };

declaration_list: Comments  declaration                  { $$ = $2; }
                 | declaration_list declaration   
                   { 
                       node* temp = $1;
                       while(temp->right != NULL) 
                          temp = temp->right;
                       temp->right = $2;
                       $$ = $1;    
                   }
                 ;

declaration: TYPE type ':' variable_list ';'  Comments
              {
                 node* typeNode = mknode($2->token, NULL, NULL); 
                 $$ = mknode("DECLERATION", typeNode, mknode($4->token,NULL,NULL));
                 $$->type = $2->type;
              }
             ;



variable_list:  variable1     { $$ = $1;}
              | variable_list ',' variable1      
              { 
                  node* temp = $1;
                  while(temp->right != NULL) 
                    temp = temp->right;
                  temp->right = $3;
                  $$ = $1;  
              }
              ;

variable1:  IDENTIFIER       // regular variable
            { 
                $$ = mknode($1, NULL, NULL);  // AST: Create node with variable name.

                //-- symbol_table - Part2 : --//
                Symbol proto = { 
                    .name = $1,
                    .kind = K_VAR, 
                    .type = T_INVALID, /* === TODO : bro we need to fix it later to the type of Array ====*/
                    .isDefined = 1,
                    .params = NULL,
                    .paramCount = 0, 
                    .line = yylineno
                    };

                if(!insert(proto)) 
                    semanticError("Variables %s redecleared", $1);
            
            }  

          | IDENTIFIER ':' literal    // initialized var
            { 
                node* idNode = mknode($1, NULL, NULL);
                $$ = mknode("=", idNode ,$3); // AST: create node "="

                //-- symbol_table - Part2 : --//
                Symbol proto = { 
                    .name = $1,
                    .kind = K_VAR, 
                    .type = $3->type,
                    .isDefined = 1,
                    .params = NULL,
                    .paramCount = 0, 
                    .line = yylineno
                    };

                if(!insert(proto)) 
                    semanticError("Variable %s redecleared", $1);

            }    

            /* ==== Variable1 : ARRAY without init! === */

          | IDENTIFIER '[' INT_LITERAL ']'     // array ( example: a[10] )
            { 
                char buffer[256];
                sprintf(buffer, "%s[%s]", $1, $3);
                $$ = mknode(buffer, NULL, NULL); 
              // AST: node (name to size)

               //-- symbol_table - Part2 : --//
                Symbol proto = { 
                    .name = $1,
                    .kind = K_VAR, 
                    .type =  T_INVALID, /* === TODO : bro we need to fix it later to the type of Array ====*/
                    .isDefined = 1,
                    .params = NULL,
                    .paramCount = 0, 
                    .line = yylineno
                    };

                if(!insert(proto)) 
                    semanticError("Variable %s redecleared", $1);

                $$->type = proto.type;    
            }

            /* ==== Variable1 : ARRAY with init! === */

          | IDENTIFIER '[' INT_LITERAL ']' ':' STRING_LITERAL   // array with value or init
            { 
               char buffer[256];
                sprintf(buffer, "%s[%s] = %s", $1, $3, $6);
                $$ = mknode(buffer, NULL, NULL); 
            
            //-- symbol_table - Part2 : --//
                Symbol proto = { 
                    .name = $1,
                    .kind = K_VAR, 
                    .type = T_INVALID, /* === TODO : bro we need to fix it later to the type of Array ====*/
                    .isDefined = 1,
                    .params = NULL,
                    .paramCount = 0, 
                    .line = yylineno
                    };

                if(!insert(proto)) 
                    semanticError("Variable %s redecleared", $1);

                $$->type = proto.type;   
            }
          ;

literal:  INT_LITERAL       { $$ = mknode($1, NULL,NULL);  $$->type = T_INT; }
        | REAL_LITERAL      { $$ = mknode($1, NULL,NULL);  $$->type = T_REAL; }
        | CHAR_LITERAL      { $$ = mknode($1, NULL,NULL);  $$->type = T_CHAR; }
        | HEX_LITERAL       { $$ = mknode($1, NULL,NULL);  $$->type = T_INT; }
        | STRING_LITERAL    { $$ = mknode($1, NULL,NULL);  $$->type = T_STRING; }
        | TRUE_LITERAL      { $$ = mknode($1, NULL,NULL);  $$->type = T_BOOL; }
        | FALSE_LITERAL     { $$ = mknode($1, NULL,NULL);  $$->type = T_BOOL; }
        | NULL_KEYWORD      { $$ = mknode($1, NULL,NULL);  $$->type = T_INVALID; }
        ; 
 
code_block: optional_var BEGIN_KEYWORD Comments inner_block Comments END_KEYWORD 
            {   
                /* SEMANTIC: OPEN NEW SCOPE*/
                pushScope();
                $$ = mknode("BLOCK", $1, $3);

    
                /* SEMANTIC: CLOSE THE SCOPE*/
                popScope();
                printScope();

            }
            ;

inner_block : optional_function_list statement_list
              {
                $$ = mknode("", $1, $2);
              }
              ;

optional_function_list:   /* empty */       { $$ = mknode("", NULL, NULL); } // We dont want segmentation fualt...
                        | function_list     { $$ = $1; }  // Return the tree that build on function_list
                        ;

optional_var :  /* empty */             { $$ = NULL; }
               | VAR declaration_list   { $$ = $2; }     //Return tree from decleration_list.
               ;

statement_list: { $$ = NULL; }
               | statement statement_list   
                 {
                  node* temp = $1;
                  while(temp->right != NULL)
                    temp = temp->right;
                  temp->right = $2;  
                  $$ = $1 ;
                 }
               ;

// about AST; each of these statments are node that we build on previous rules, so we return it back and dont create new tree 
statement:    simple_statement      { $$ = $1; }
            | call_statement        { $$ = $1; }
            | if_statement          { $$ = $1; }
            | while_statement       { $$ = $1; }
            | do_while_statement    { $$ = $1; }
            | for_statement         { $$ = $1; }
            | code_block            { $$ = $1; }
            ;


/* This is the left side of the assignment... */
lvalue:   IDENTIFIER 
            { 
                $$ = mknode($1, NULL, NULL); 

                /* Check for existing ====== VARIABLE ===== */
                Symbol *s = lookup($1);
                if(!s || (s->kind !=K_VAR && s->kind != K_PARAM) )
                    semanticError("Unknown variable %s", $1);
                
                /* Save type of value */
                $$->type = s->type;    
            }

        | IDENTIFIER '[' expression ']'  // for example of a[19] that makes us a problem
            { 
                // AST: we want IDENTIFIER and ARRAY-INDEX
                $$ = mknode("ARRAY_ACCESS", mknode($1, NULL, NULL), $3); 
                
                /* Check for existing ====== ARRAY ===== */
                Symbol *s = lookup($1);
                if(!s || (s->kind !=K_VAR && s->kind != K_PARAM) )
                    semanticError("Unknown array %s", $1);
                
                /* Save type of value */
                $$->type = s->type; 
            } 
        
        
        | '*' IDENTIFIER               // For pointers, For example: *x
            { 
                // AST: pointer approach
                $$ = mknode("POINTER_ACCESS", mknode($2, NULL, NULL), NULL); 
                   /* Check for existing ====== POINTER ===== */
                Symbol *s = lookup($2);
                if(!s || !isPointer(s->type) )
                    semanticError("%s IS NOT A POINTER", $2);
                
                /* Save type of value */
                $$->type = semTypeOfNode($$); 
            } 
        
        
        | '&' IDENTIFIER               // For pointers, For example: *x
            { 
                // AST: pointer approach
                $$ = mknode("POINTER_ACCESS", mknode($2, NULL, NULL), NULL); 

                     /* Check for existing ====== VARIABLE ===== */
                Symbol *s = lookup($2);
                if(!s)
                    semanticError("Unknown variable %s", $2);
                
                /* Save type of value */
                $$->type = semTypeOfNode($$); 
            } 
        ; 

simple_statement: lvalue '=' expression ';'  Comments  
                    { 
                        //-- symbol_table - Part2 : --//
                        Type lhs = semTypeOfLValue($1);
                        Type rhs = semTypeOfNode($3);
                        if(!semCheckAssign(lhs,rhs))
                            semanticError("Type mismatch in assignment");

                        // make AST:    
                        $$ = mknode("=", $1, $3); 
                        $$->type = lhs;
                    }

                 | return_statement             { $$ = $1; }
                 ;

return_statement: RETURN_KEYWORD expression ';' 
                    {
                         //-- symbol_table - Part2 : --//
                         Type ret = semTypeOfNode($2); // check type of return 
                         if(!semCheckReturn(ret))
                            semanticError("Invalid return type");

                         // AST build :    
                         $$ = mknode("RET", $2, NULL); 
                         $$->type = ret; 
                    }
                 ;

/* For procedure only, for example: call foo(), for function is in line 300 */
call_statement :   CALL IDENTIFIER '('  ')' ';' // Procedure without parameter
                     {
                        //-- symbol_table - Part2 : --//
                        Symbol *fn = lookup($2);
                        if(!fn || fn->kind != K_FUNC)
                            semanticError("%s is not a function", $2);

                        if(!semCheckCall(fn, NULL))
                            semanticError("Wrong args for %s", $2);

                        //AST build :        
                        $$ = mknode("CALL", mknode($2, NULL, NULL), NULL); 
                        $$->type = fn->type;
                     }

                 | CALL IDENTIFIER '(' expression_list ')' ';'    // Procedure with parameter
                     { 
                        //-- symbol_table - Part2 : --//
                        Symbol *fn = lookup($2);
                        if(!fn || fn->kind != K_FUNC)
                            semanticError("%s is not a function", $2);

                        if(!semCheckCall(fn, $4))
                            semanticError("Wrong args for %s", $2);

                        //AST build : 
                        $$ = mknode("CALL", mknode($2, NULL, NULL), $4); 
                        $$->type = fn->type;
                     }
                 ;

if_statement: IF expression ':' single_block_or_block elif_list else_block 
            {
                //-- symbol_table - Part2 : --//
                /* ==== we want to check if the condition is Boolean: === */

                Type cond = semTypeOfNode($2);
                if(!semCheckCondition(cond))
                    semanticError("IF condition is not boolean");

                // AST build node IF/IF-ELSE/ELSE-IF    
                node* if_node = mknode("IF", $2, $4);
                if ($5 != NULL)
                    if_node = mknode("IF-ELSE", if_node, $5);
                if ($6 != NULL)
                    if_node = mknode("IF_ELIF_ELSE", if_node, $6);
                $$ = if_node;
            }
            ;

elif_list:  /* empty */   {$$ = NULL;}  /*{ $$ = mknode("LINE330FIX", NULL, NULL); }*/ 
            | elif_blocks   { $$ = $1; }
            ;

elif_blocks:  elif_block                 { $$ = $1; }   // AST: if theres only one block - return it.
            | elif_blocks elif_block     
            {
                node* temp = $1;
                while(temp->right != NULL)
                   temp = temp->right;
                temp->right = $2;  
                $$ = $1 ;
            }
              // AST: if have more than 1 - create node to each block
            ;

elif_block: ELIF expression ':' single_block_or_block  
            { 
                /* semantic: Check ELIF bool */
                Type cond = semTypeOfNode($2);
                if(!semCheckCondition(cond))
                    semanticError("ELIF condition is not boolean");

                //Build ELIF node(part-1):
                $$ = mknode("ELIF", $2, $4); 
            } 
            ;

else_block:  /* empty */         { $$ = NULL; }     /*{ $$ = mknode("NO_ELSE", NULL, NULL); } */
            | ELSE ':' single_block_or_block    { $$ = mknode("ELSE", $3, NULL); }
            ;

single_block_or_block:  simple_statement   { $$ = $1; } 
                       | code_block        { $$ = $1; } 
                       ;

while_statement: WHILE expression ':' single_block_or_block  
                { 
                    /* Semantic: WHILE cond must be boolean*/
                    Type cond = semTypeOfNode($2);
                    if ( cond != T_BOOL)
                        semanticError("WHILE condition is not boolean");

                    // AST + Type void    
                    $$ = mknode("WHILE", $2, $4); 
                    $$->type = T_VOID;
                }
                 ;

// AST: we build DO ( from optional_var and statment_list ) and then connected it to DO_WHILE !
// for shimi: i changed this rule from BEGIN_KEYWORD statement_LIST END_KEYWORD ---> to : code_block

do_while_statement:  DO ':' optional_var code_block  WHILE  expression ';' 
                      {
                      /* Semantic: DO-WHILE cond must be boolean*/
                        Type cond = semTypeOfNode($6);
                        if ( cond != T_BOOL)
                            semanticError("DO-WHILE condition is not boolean");

                        $$ = mknode("DO_WHILE", $4, $6);
                        $$->type = T_VOID;
                      }
                      ;


for_statement: FOR '(' IDENTIFIER '=' expression ';' expression ';' IDENTIFIER '=' expression ')' ':' single_block_or_block 
                {
                    /* Semantic: INIT and UPDATE are assigment for exp*/
                    Type lhs1 = semTypeOfLValue(mknode($3, NULL,NULL));
                    Type rhs1 = semTypeOfNode($5);
                    if( lhs1 != rhs1 && !(isNumeric(lhs1)) && !(isNumeric(rhs1)) )
                        semanticError("there is Type mismatch in for-init");

                    Type condType = semTypeOfNode($7);
                    if( condType != T_BOOL )
                        semanticError("FOR condition is not boolean");    

                    Type lhs2 = semTypeOfLValue(mknode($9, NULL,NULL));
                    Type rhs2 = semTypeOfNode($11);
                    if( lhs2 != rhs2 && !(isNumeric(lhs2)) && !(isNumeric(rhs2)) )
                        semanticError("there is Type mismatch in for-update");

                    node* init = mknode("=", mknode($3, NULL, NULL), $5); 
                    node* condNode = $7;
                    node* update = mknode("=", mknode($9, NULL, NULL), $11);
                    node* control = mknode("FOR_CTRL", init, mknode("COND_UPDATE", condNode, update));
                    $$ = mknode("FOR",control, $14);
                    $$->type = T_VOID;
                }
              ;

expression: simple_expression               { $$ = $1; } 
           | expression '+' expression      
                    { 
                        // SEMANTIC: Check types of both expression 
                        Type a = semTypeOfNode($1), b = semTypeOfNode($3);
                        Type r = resultBinary('+', a, b);
                        if ( r == T_INVALID )
                            semanticError("Invalid types for +");

                        // NODE for + ;    
                        $$ = mknode("+", $1, $3); 
                        $$->type = r;
                    }

           | expression '-' expression      
                    { 
                       // SEMANTIC: Check types of both expression 
                        Type a = semTypeOfNode($1), b = semTypeOfNode($3);
                        Type r = resultBinary('-', a, b);
                        if ( r == T_INVALID )
                            semanticError("Invalid types for -");

                        // NODE for - ;    
                        $$ = mknode("-", $1, $3); 
                        $$->type = r;
                    } 

           | expression '*' expression     
                    { 
                       // SEMANTIC: Check types of both expression 
                        Type a = semTypeOfNode($1), b = semTypeOfNode($3);
                        Type r = resultBinary('*', a, b);
                        if ( r == T_INVALID )
                            semanticError("Invalid types for *");

                        // NODE for * ;    
                        $$ = mknode("*", $1, $3); 
                        $$->type = r;
                    } 

           | expression '/' expression     
                    { 
                       // SEMANTIC: Check types of both expression 
                        Type a = semTypeOfNode($1), b = semTypeOfNode($3);
                        Type r = resultBinary('-', a, b);
                        if ( r == T_INVALID )
                            semanticError("Invalid types for /");

                        // NODE for / ;    
                        $$ = mknode("/", $1, $3); 
                        $$->type = r;
                    } 

           | expression '<' expression     
                        { 
                       // SEMANTIC: Check types of both expression 
                        Type a = semTypeOfNode($1), b = semTypeOfNode($3);
                        Type r = resultBinary('<', a, b);
                        if ( r == T_INVALID )
                            semanticError("Invalid types for <");

                        // NODE for < ;    
                        $$ = mknode("<", $1, $3); 
                        $$->type = r;
                    } 
           | expression '>' expression     
                    { 
                       // SEMANTIC: Check types of both expression 
                        Type a = semTypeOfNode($1), b = semTypeOfNode($3);
                        Type r = resultBinary('>', a, b);
                        if ( r == T_INVALID )
                            semanticError("Invalid types for >");

                        // NODE for > ;    
                        $$ = mknode(">", $1, $3); 
                        $$->type = r;
                    } 

           | '|' expression '|'             
                    // For size of variable //
                    { 
                        Type a = semTypeOfNode($2);
                        Type r = resultUnary('|',a);
                        if( r == T_INVALID)
                            semanticError("Invalid type for |expr| ");
                        $$ = mknode("SIZEOF", $2, NULL); 
                        $$->type = r;    
                    } 

           | expression DOUBLE_EQUAL expression     
                    { 
                       // SEMANTIC: Check types of both expression 
                        Type a = semTypeOfNode($1), b = semTypeOfNode($3);
                        Type r = resultBinary(DOUBLE_EQUAL, a, b);    // we need to Test if is possible to put the operator '==' itself 
                        if ( r == T_INVALID )
                            semanticError("Invalid types for ==");

                        // NODE for == ;    
                        $$ = mknode("==", $1, $3); 
                        $$->type = r;
                    } 


           | expression NOT_EQUAL expression         
                    { 
                       // SEMANTIC: Check types of both expression 
                        Type a = semTypeOfNode($1), b = semTypeOfNode($3);
                        Type r = resultBinary(NOT_EQUAL, a, b);    // we need to Test if is possible to put the operator '!=' itself 
                        if ( r == T_INVALID )
                            semanticError("Invalid types for !=");

                        // NODE for != ;    
                        $$ = mknode("!=", $1, $3); 
                        $$->type = r;
                    } 

           | expression GREATER_EQUAL expression     
                    { 
                       // SEMANTIC: Check types of both expression 
                        Type a = semTypeOfNode($1), b = semTypeOfNode($3);
                        Type r = resultBinary(GREATER_EQUAL, a, b);    // we need to Test if is possible to put the operator '>=' itself 
                        if ( r == T_INVALID )
                            semanticError("Invalid types for >=");

                        // NODE for >= ;    
                        $$ = mknode(">=", $1, $3); 
                        $$->type = r;
                    }

           | expression LESS_EQUAL expression       
                    { 
                       // SEMANTIC: Check types of both expression 
                        Type a = semTypeOfNode($1), b = semTypeOfNode($3);
                        Type r = resultBinary(LESS_EQUAL, a, b);    // we need to Test if is possible to put the operator '<=' itself 
                        if ( r == T_INVALID )
                            semanticError("Invalid types for <=");

                        // NODE for <= ;    
                        $$ = mknode("<=", $1, $3); 
                        $$->type = r;
                    }

           | expression AND expression               
                    { 
                       // SEMANTIC: Check types of both expression 
                        Type a = semTypeOfNode($1), b = semTypeOfNode($3);
                        Type r = resultBinary(AND, a, b); 
                        if ( r == T_INVALID )
                            semanticError("Invalid types for AND");

                        // NODE for AND ;    
                        $$ = mknode("AND", $1, $3); 
                        $$->type = r;
                    }

           | expression OR expression               
                    { 
                       // SEMANTIC: Check types of both expression 
                        Type a = semTypeOfNode($1), b = semTypeOfNode($3);
                        Type r = resultBinary(OR, a, b);   
                        if ( r == T_INVALID )
                            semanticError("Invalid types for OR");

                        // NODE for OR ;    
                        $$ = mknode("OR", $1, $3); 
                        $$->type = r;
                    }

           | NOT expression                          
                    { 
                        Type a = semTypeOfNode($2);
                        Type r = resultUnary(NOT,a);
                        if( r == T_INVALID)
                            semanticError("Invalid type for NOT");
                        $$ = mknode("NOT", $2, NULL); 
                        $$->type = r;
                    }
                  
           | call_expression                         { $$ = $1; }
          ;

simple_expression: '(' expression ')'                 { $$ = $2; }
                    | IDENTIFIER '[' expression ']'    
                        {  
                             $$ = mknode("ARRAY_ACCESS", mknode($1, NULL, NULL), $3);
                             Symbol *s = lookup($1); 
                             if( !s || (s->kind != K_VAR && s->kind != K_PARAM) )
                                semanticError("Unknown array %s", $1);
                             $$->type = s->type;
                        }

                    | pointer_expression               { $$ = $1; }

                    | literal                          { $$ = $1; }

                    | IDENTIFIER                       
                        { 
                            $$ = mknode($1, NULL, NULL); 
                            Symbol *s = lookup($1); 
                             if( !s )
                                semanticError("Unknown Variable %s", $1);
                             $$->type = s->type;
                        }    

                    | IDENTIFIER '(' ')'                    
                        { 
                            Symbol *s = lookup($1);
                            if( !s || s->kind != K_FUNC )
                                semanticError("%s is not a function", $1);
                            $$ = mknode("CALL", mknode($1, NULL, NULL), NULL); 
                            $$->type = s->type;
                        }


                    | IDENTIFIER '(' expression_list ')'    
                        { 
                            Symbol *s = lookup($1);
                            if( !s || s->kind != K_FUNC )
                                semanticError("%s is not a function", $1);
                            $$ = mknode("CALL", mknode($1, NULL, NULL), $3); 
                            $$->type = s->type;
                        }
                    ;

expression_list:  expression     
                { 
                    $$ = $1; 
                }

                | expression_list ',' expression      
                { 
                   node* temp = $1;
                   while(temp->right != NULL)
                     temp = temp->right;
                   temp->right = $3;  
                   $$ = $1 ;
                }
                ;

/* For option to call func with assign, for example: x = call foo(a,b) */
call_expression:   CALL IDENTIFIER '('  ')'                 
                    { 
                        Symbol *s = lookup($2);
                        if( !s || s->kind != K_FUNC )
                                semanticError("%s is not a function", $2);
                        $$ = mknode("CALL", mknode($2, NULL,NULL), NULL);
                        $$->type = s->type;
                    }

                 | CALL IDENTIFIER '(' expression_list ')' 
                      { 
                        Symbol *s = lookup($2);
                        if( !s || s->kind != K_FUNC )
                                semanticError("%s is not a function", $2);
                        $$ = mknode("CALL", mknode($2, NULL, NULL), $4); 
                        $$->type = s->type;
                    }                     
                 ;

pointer_expression:   '*' simple_expression  // we change IDENTIFIER to simple_expression to support : *y[5]
                        { 
                            /* Type a = semTypeOfNode($2);
                            if(!isPointer(a))
                                semanticError("Cannot dereference non-pointer");
                            Type r = resultUnary('*', a); */
                            $$ = mknode("DEREF", $2, NULL); 
                            /*$$->type = r; */
                        }

                    | '&' simple_expression  // we change IDENTIFIER to simple_expression to support : &y[5]
                        { 
                            /* Type a = semTypeOfNode($2);
                            if(!(a != T_INVALID)) /* every accept type */
                            /* Type r = resultUnary('&', a); */
                            $$ = mknode("ADDRESS", $2, NULL); 
                            /* $$->type = r; */    
                        }
                    ; 

%%

#include <stdio.h>

int yyerror(char *s) {
    printf("\nSyntax error at line %d: %s got %s\n", yylineno, s, yytext);
    return 0;
}


node* mknode(char *token, node *left, node *right) {
    node *newnode = (node*)malloc(sizeof(node));
        if(!newnode){
            printf("Memory Allocation Failed");
            exit(1);
        }
    if(token != NULL) {
         char *newstr = (char*)malloc(strlen(token) + 1);
         if(!newstr) {
            printf("Memory Allocation for token failed.");
            exit(1);
         }
         strcpy(newstr, token);
         newnode->token = newstr;
    }    else {
            newnode->token = NULL;
    }
   
    newnode->left = left;
    newnode->right = right;
    return newnode;
}

void printtree(node* tree, int level) {
    if (tree == NULL) return;

    int has_token = (tree->token != NULL && tree->token[0] != '\0');
    int is_leaf = (tree->left == NULL && tree->right == NULL);

    if (!has_token) {
        if (tree->left)  printtree(tree->left, level);
        if (tree->right) printtree(tree->right, level);
        return;
    }

    for (int i = 0; i < level; i++) printf("  ");

    if (is_leaf) {
        printf("(%s)\n", tree->token);
        return;
    }

    printf("(%s\n", tree->token);

    if (tree->left)  printtree(tree->left, level + 1);
    if (tree->right) printtree(tree->right, level + 1);

    for (int i = 0; i < level; i++) printf("  ");
    printf(")\n");
}






int main() {
    printf("Starting to parse input ..... \n");
    
    // this is for see the scope :
    semInit();
    //printScope();

    // continue: 
    int result = yyparse();

    if (result == 0) {
        printf("\nYEAH! Parse Successful!\n");
        printf("Here is the AST tree:\n\n");
        if (root != NULL) {
            printtree(root, 0);
            printf("\n");
        } else {
            printf("No AST generated.\n");
        }
    } else {
        printf("Parse Failed.\n");
    }
    return result;
}


