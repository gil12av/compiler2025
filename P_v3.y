%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int yylex(void);
int yyerror(char *s);
int yydebug = 1;
extern int yylineno; // בשביל לעקוב אחר מספר שורה
extern char *yytext;

typedef struct node {
    char *token;
    struct node *left;
    struct node *right;
} node;

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
void printtree(node *tree, int level) {
    if (tree == NULL) return;

    int has_token = (tree->token != NULL && strcmp(tree->token, "") != 0);
    int has_children = (tree->left != NULL || tree->right != NULL);

    if (!has_token && !has_children)
        return;

    if (!has_token) {
        if (tree->left) printtree(tree->left, level);
        if (tree->right) printtree(tree->right, level);
        return;
    }

    
    for (int i = 0; i < level; i++)
        printf(" ");

    if (has_children) printf("(");

    printf("%s", tree->token);

    if (tree->left) {
        printf("\n");
        printtree(tree->left, level + 1);
    }

    if (tree->right) {
        printf("\n");
        printtree(tree->right, level + 1);
    }

    if (has_children) printf(")");
    
}



node* root = NULL;


%}

%union {
    char *string;
    struct node* node;
}

%left '+' '-'
%left '*' '/'
%left '<' '>' DOUBLE_EQUAL NOT_EQUAL GREATER_EQUAL LESS_EQUAL

%left OR
%left AND
%right NOT

%token <string> BOOL CHAR INT REAL STRING
%token <string> INT_PTR CHAR_PTR REAL_PTR
%token <string> IF ELIF ELSE WHILE FOR
%token <string> TYPE VAR PAR RETURN_KEYWORD NULL_KEYWORD DO RETURNS
%token <string> BEGIN_KEYWORD END_KEYWORD DEF CALL
%token <string> TRUE_LITERAL FALSE_LITERAL
%token <string> HEX_LITERAL INT_LITERAL REAL_LITERAL STRING_LITERAL CHAR_LITERAL
%token <string> IDENTIFIER


%type <node> program function_list function return_value parameter_list parameter
%type <node> type declaration_list declaration variable_list variable1
%type <node> literal code_block inner_block optional_function_list optional_var
%type <node> statement_list statement lvalue simple_statement return_statement
%type <node> call_statement if_statement elif_list elif_blocks elif_block else_block
%type <node> single_block_or_block while_statement do_while_statement for_statement
%type <node> experssion simple_expression experssion_list pointer_experssion call_experssion


%start program

%%

program: function_list  
        { 
            $$ = mknode("CODE", $1, NULL); 
            root = $$;
            
        } 
        ;

function_list:  function                    { $$ = $1; }
              | function_list function      { $$ = mknode("FUNC_LIST", $1, $2); }
              ;


function: DEF IDENTIFIER '(' parameter_list ')' ':' return_value code_block 
            {
                node* name = mknode($2, NULL, NULL);    // Function name
                node* pars = mknode("PARS", $4, NULL);  // Pararmeter
                node* ret = mknode("RET", $7, NULL);    // Return_value
                node* body = mknode("BODY", $8, NULL);  // Code_block 

                node* func_def = mknode("", name, pars);
                node* temp = mknode("", ret, body);
                $$ = mknode("FUNC",func_def, temp);
            } 
            ;

return_value :  /*empty*/       { $$ = mknode("NONE", NULL, NULL); }         
               | RETURNS type   { $$ = mknode("",mknode($2->token, NULL,NULL), NULL); }    
               ;

// about ast: if there just 1 parameter, we return PARS and if theres more than 1 we create node PARS
parameter_list: /*empty*/                       { $$ = mknode("", NULL, NULL); }                      
                | parameter                     { $$ = $1; } 
                | parameter_list ';' parameter  { $$ = mknode("", $1, $3); }  
                ;

// about ast: we want to print example: (par1 INT x) --> so we use buffer.
parameter: PAR type ':' IDENTIFIER  
           { 
                char buffer[256];
                sprintf(buffer, "%s %s %s", $1, $2->token, $4);
                $$ = mknode(buffer, NULL, NULL);
           }
           ;

// about ast: each of types are return from scanner to PAR or return_value.
type:  INT          { $$ = mknode("INT", NULL,NULL); }
     | REAL         { $$ = mknode("REAL", NULL,NULL); }
     | CHAR         { $$ = mknode("CHAR", NULL,NULL); }
     | STRING       { $$ = mknode("STRING", NULL,NULL); }
     | BOOL         { $$ = mknode("BOOL", NULL,NULL); }
     | INT_PTR      { $$ = mknode("INT_PTR", NULL,NULL); }
     | CHAR_PTR     { $$ = mknode("CHAR_PTR", NULL,NULL); }
     | REAL_PTR     { $$ = mknode("REAL_PTR", NULL,NULL); }
     ;

declaration_list:  declaration                    { $$ = $1; }
                 | declaration_list declaration   { $$ = mknode("LINE175", $1, $2); }
                 ;

declaration: TYPE type ':' variable_list ';'     { $$ = mknode("", $2, $4); }
             ;

variable_list:  variable1                         { $$ = $1;}
              | variable_list ',' variable1      { $$ = mknode("LINE182", $1, $3); }
              ;

variable1:  IDENTIFIER                                          // regular variable
            { $$ = mknode($1, NULL, NULL); }                    // AST: Create node with variable name.

          | IDENTIFIER ':' literal                              // initialized var
            { $$ = mknode("=",mknode($1, NULL, NULL), $3); }    // AST: create node "="

          | IDENTIFIER '[' INT_LITERAL ']'                      // array (example: a[10] )
            { $$ = mknode("ARRAY", mknode($1, NULL, NULL),mknode($3, NULL, NULL)); }  // AST: node (name to size)

          | IDENTIFIER '[' INT_LITERAL ']' ':' STRING_LITERAL   // array with value or init
            { 
              node* array_node = mknode("ARRAY", mknode($1, NULL, NULL),mknode($3, NULL,NULL));
              $$ = mknode("=", array_node, mknode($6,NULL,NULL));  
            }
          ;

literal:  INT_LITERAL       { $$ = mknode($1, NULL,NULL); }
        | REAL_LITERAL      { $$ = mknode($1, NULL,NULL); }
        | CHAR_LITERAL      { $$ = mknode($1, NULL,NULL); }
        | HEX_LITERAL       { $$ = mknode($1, NULL,NULL); }
        | STRING_LITERAL    { $$ = mknode($1, NULL,NULL); }
        | TRUE_LITERAL      { $$ = mknode($1, NULL,NULL); }
        | FALSE_LITERAL     { $$ = mknode($1, NULL,NULL); }
        | NULL_KEYWORD      { $$ = mknode($1, NULL,NULL); }
        ; 
 
code_block: optional_var BEGIN_KEYWORD inner_block END_KEYWORD 
            {   
                $$ = mknode("", $1, $3);
            }
            ;

inner_block : optional_function_list statement_list
              {
                $$ = mknode("BLOCK", $1, $2);
              }
              ;

optional_function_list:   /* empty */       { $$ = mknode("LINE223", NULL, NULL); } // We dont want segmentation fualt...
                        | function_list     { $$ = $1; }  // Return the tree that build on function_list
                        ;

optional_var :  /* empty */             { $$ = mknode("",NULL, NULL); }
               | VAR declaration_list   { $$ = mknode("VAR",$2, NULL); } //Return tree from decleration_list.
               ;

statement_list:  statement                  { $$ = $1; }
               | statement statement_list   { $$ = mknode("STATMENTS", $1, $2); }
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

 /*
lvalueFix:   IDENTIFIER 
        | IDENTIFIER '[' boolean? ']' ; // need to fix case of operator ( REAL,BOOL, NOT_EQUAL etc'...)
*/

/* This is the left side of the assignment... */
lvalue:   IDENTIFIER 
            { $$ = mknode($1, NULL, NULL); }
        | IDENTIFIER '[' experssion ']'  // for example of a[19] that makes us a problem
            { $$ = mknode("ARRAY_ACCESS", mknode($1, NULL, NULL), $3); } // AST: we want IDENTIFIER and ARRAY-INDEX
        | '*' IDENTIFIER               // For pointers, For example: *x
            { $$ = mknode("POINTER_ACCESS", mknode($2, NULL, NULL), NULL); } // AST: pointer approach
        ; 

simple_statement:  lvalue '=' experssion ';'    { $$ = mknode("=", $1, $3); }
                 | return_statement             { $$ = $1; }
                 ;

return_statement: RETURN_KEYWORD experssion ';' { $$ = mknode("RET", $2, NULL); }
                 ;

/* For procedure only, for example: call foo(), for function is in line 300 */
call_statement :   CALL IDENTIFIER '('  ')' ';' // Procedure without parameter
                     { $$ = mknode("CALL", mknode($2, NULL, NULL), NULL); }
                 | CALL IDENTIFIER '(' experssion_list ')' ';'    // Procedure with parameter
                     { $$ = mknode("CALL", mknode($2, NULL, NULL), $4); }
                 ;

if_statement:IF experssion ':' single_block_or_block elif_list else_block 
            {
                node* if_node = mknode("", $2, $4);
                node* elif_node = $5;   // save _tree of elif or elifs
                node* else_node = $6;   // save _tree of else or elses

                if (elif_node) {    // want to check if theres elif.
                    $$ = mknode("ELIF", if_node, mknode("", elif_node, else_node));
                } else {
                    $$ = mknode("IF-ELSE", if_node,else_node);
                }
            }
            ;

elif_list:  /* empty */     { $$ = mknode("", NULL, NULL); }
            | elif_blocks   { $$ = $1; }
            ;

elif_blocks:  elif_block                 { $$ = $1; }   // AST: if theres only one block - return it.
            | elif_blocks elif_block     { $$ = mknode("", $1, $2); }  // AST: if have more than 1 - create node to each block
            ;

elif_block: ELIF experssion ':' single_block_or_block  { $$ = mknode("", $2, $4); } 
            ;

else_block:  /* empty */                        { $$ = mknode("", NULL, NULL); }
            | ELSE ':' single_block_or_block    { $$ = mknode("ELSE", $3, NULL); }
            ;

single_block_or_block:  simple_statement   { $$ = $1; } 
                       | code_block        { $$ = $1; } 
                       ;

while_statement: WHILE experssion ':' single_block_or_block  { $$ = mknode("WHILE", $2, $4); }
                 ;

// AST: we build DO ( from optional_var and statment_list ) and then connected it to DO_WHILE !
do_while_statement:  DO ':' optional_var BEGIN_KEYWORD statement_list END_KEYWORD WHILE  experssion ';' 
                    {
                        node* body = mknode("BODY", $3, $5);
                        $$ = mknode("DO_WHILE", body, mknode($7, NULL, NULL));
                    }
                    ;

for_statement: FOR '(' IDENTIFIER '=' experssion ';' experssion ';' IDENTIFIER '=' experssion ')' ':' single_block_or_block 
                {
                    node* init = mknode("=", mknode($3, NULL, NULL), $5); 
                    node* cond = $7;
                    node* update = mknode("=", mknode($9, NULL, NULL), $11);
                    node* control = mknode("", init, mknode("", cond, update));
                    $$ = mknode("FOR",control, $14);
                }
              ;

experssion: simple_expression               { $$ = $1; } 
           | experssion '+' experssion      { $$ = mknode("+", $1, $3); } 
           | experssion '-' experssion      { $$ = mknode("-", $1, $3); } 
           | experssion '*' experssion      { $$ = mknode("*", $1, $3); } 
           | experssion '/' experssion      { $$ = mknode("/", $1, $3); } 
           | experssion '<' experssion      { $$ = mknode("<", $1, $3); } 
           | experssion '>' experssion      { $$ = mknode(">", $1, $3); } 
           | '|' experssion '|'             { $$ = mknode("ABS", $2, NULL); } // For size of variable //
           | experssion DOUBLE_EQUAL experssion      { $$ = mknode("==", $1, $3); }
           | experssion NOT_EQUAL experssion         { $$ = mknode("!=", $1, $3); }
           | experssion GREATER_EQUAL experssion     { $$ = mknode(">=", $1, $3); }
           | experssion LESS_EQUAL experssion        { $$ = mknode("<=", $1, $3); }
           | experssion AND experssion               { $$ = mknode("AND", $1, $3); }
           | experssion OR experssion                { $$ = mknode("OR", $1, $3); }
           | NOT experssion                          { $$ = mknode("NOT", $2, NULL); }
           | call_experssion                         { $$ = $1; }
          ;

simple_expression: '(' experssion ')'                  { $$ = $2; }
                    | IDENTIFIER '[' experssion ']'    { $$ = mknode("ARRAY_ACCESS", mknode($1, NULL, NULL), $3); }
                    | pointer_experssion               { $$ = $1; }
                    | literal                          { $$ = $1; }
                    | IDENTIFIER                       { $$ = mknode($1, NULL, NULL); }
                    ;

experssion_list:  experssion                          { $$ = $1; }
                | experssion_list ',' experssion      { $$ = mknode("ARGS", $1, $3); }
                ;

/* For option to call func with assign, for example: x = call foo(a,b) */
call_experssion:   CALL IDENTIFIER '('  ')'                 { $$ = mknode("CALL", mknode($2, NULL,NULL), NULL);}
                 | CALL IDENTIFIER '(' experssion_list ')' { $$ = mknode("CALL", mknode($2, NULL, NULL), $4); }
                 ;

pointer_experssion:   '*' simple_expression  // we change IDENTIFIER to simple_expression to support : *y[5]
                        { $$ = mknode("DEREF", $2, NULL); }
                    | '&' simple_expression  // we change IDENTIFIER to simple_expression to support : &y[5]
                        { $$ = mknode("ADDRESS", $2, NULL); }
                    ; 

%%

#include <stdio.h>

int yyerror(char *s) {
    printf("\nSyntax error at line %d: %s got %s\n", yylineno, s, yytext);
    return 0;
}



int main() {
    printf("Starting to parse input ..... \n");
    if (yyparse() == 0) {
        printf("\nYEAH! Parse Successful!\n");
        printtree(root, 0);
        printf("\n");
    } else {
        printf("Parse Failed.\n");
    }
    return 0;
}


