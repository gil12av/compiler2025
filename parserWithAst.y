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

node *mknode(char *token, node *left, node *right);
void printtree(node *tree);
node* root = NULL;
#define YYSTYPE struct node*
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
%token IF ELIF ELSE WHILE FOR
%token TYPE VAR PAR RETURN_KEYWORD NULL_KEYWORD DO RETURNS
%token BEGIN_KEYWORD END_KEYWORD DEF CALL
%token <string> TRUE_LITERAL FALSE_LITERAL
%token <string> HEX_LITERAL INT_LITERAL REAL_LITERAL STRING_LITERAL CHAR_LITERAL
%token <string> IDENTIFIER

/*
%type <node> program function_list function return_value parameter_list parameter
%type <node> type declaration_list declaration variable_list variable1
%type <node> literal code_block inner_block optional_function_list optional_var
%type <node> statement_list statement lvalue simple_statement return_statement
%type <node> call_statement if_statement elif_list elif_blocks elif_block else_block
%type <node> single_block_or_block while_statement do_while_statement for_statement
%type <node> experssion simple_expression experssion_list pointer_experssion
*/

%type <node> program function_list 
%type function return_value parameter_list parameter
%type type declaration_list declaration variable_list variable1
%type literal code_block inner_block optional_function_list optional_var
%type statement_list statement lvalue simple_statement return_statement
%type call_statement if_statement elif_list elif_blocks elif_block else_block
%type single_block_or_block while_statement do_while_statement for_statement
%type experssion simple_expression experssion_list pointer_experssion



%start program

%%

program: function_list  { $$ = mknode("CODE", NULL, $1);
                            root = $$; 
                            printtree(root);                          
                        }
        ;

function_list:  function                
              | function_list function  
              ;


function: DEF IDENTIFIER '(' parameter_list ')' ':' return_value code_block 
            
            ;

return_value :  /*empty*/           
               | RETURNS type      
               ;

// about ast: if there just 1 parameter, we return PARS and if theres more than 1 we create node PARS
parameter_list: /*empty*/                         
                | parameter                       
                | parameter_list ';' parameter    
                ;

// about ast: we want to print example: (par1 INT x) --> so we use buffer.
parameter: PAR type ':' IDENTIFIER 
            ;

// about ast: each of types are return from scanner to PAR or return_value.
type:  INT       
     | REAL      
     | CHAR      
     | STRING    
     | BOOL      
     | INT_PTR   
     | CHAR_PTR  
     | REAL_PTR  
     ;

declaration_list: declaration | declaration_list declaration ;

declaration: TYPE type ':' variable_list ';' ;

variable_list: variable1 | variable_list ',' variable1 ;

variable1:  IDENTIFIER                                          // regular variable
          | IDENTIFIER ':' literal                              // initialized var
          | IDENTIFIER '[' INT_LITERAL ']'                      // array (example: a[10] )
          | IDENTIFIER '[' INT_LITERAL ']' ':' STRING_LITERAL   // array with value or init
          ;

literal: INT_LITERAL | REAL_LITERAL | CHAR_LITERAL | HEX_LITERAL
        | STRING_LITERAL | TRUE_LITERAL | FALSE_LITERAL | NULL_KEYWORD ; 
 
code_block: optional_var BEGIN_KEYWORD inner_block END_KEYWORD ;

inner_block : optional_function_list statement_list;

optional_function_list: /* empty */  | function_list;

optional_var : /* empty */ | VAR declaration_list ;

statement_list: statement |  statement statement_list ;

statement:    simple_statement  
            | call_statement
            | if_statement
            | while_statement 
            | do_while_statement
            | for_statement
            | code_block
            ;

 /*
lvalueFix:   IDENTIFIER 
        | IDENTIFIER '[' boolean? ']' ; // need to fix case of operator ( REAL,BOOL, NOT_EQUAL etc'...)
*/

/* This is the left side of the assignment... */
lvalue:   IDENTIFIER 
        | IDENTIFIER '[' experssion ']'  // for example of a[19] that makes us a problem
        | '*' IDENTIFIER ;               // For pointers, For example: *x
        ; 

simple_statement:  lvalue '=' experssion ';' 
                 | return_statement; 

return_statement: RETURN_KEYWORD experssion ';';

/* For procedure only, for example: call foo(), for function is in line 150 */
call_statement :   CALL IDENTIFIER '('  ')' ';' // Procedure without parameter
                 | CALL IDENTIFIER '(' experssion_list ')' ';'    // Procedure with parameter

if_statement:IF experssion ':' single_block_or_block elif_list else_block ;

elif_list:
    /* empty */ | elif_blocks ;

elif_blocks:
    elif_block | elif_blocks elif_block ;

elif_block:
    ELIF experssion ':' single_block_or_block ;

else_block:
    /* empty */ | ELSE ':' single_block_or_block ;

single_block_or_block:
    simple_statement | code_block ;

while_statement: WHILE experssion ':' single_block_or_block;

do_while_statement:  DO ':' optional_var BEGIN_KEYWORD statement_list END_KEYWORD WHILE  experssion ';' ;

for_statement:FOR '(' IDENTIFIER '=' experssion ';' experssion ';' IDENTIFIER '=' experssion ')' ':' single_block_or_block ;

experssion: simple_expression
           | experssion '+' experssion
           | experssion '-' experssion
           | experssion '*' experssion
           | experssion '/' experssion
           | experssion '<' experssion
           | experssion '>' experssion
           | '|' experssion '|'         // For size of variable
           | experssion DOUBLE_EQUAL experssion
           | experssion NOT_EQUAL experssion
           | experssion GREATER_EQUAL experssion
           | experssion LESS_EQUAL experssion
           | experssion AND experssion
           | experssion OR experssion
           | NOT experssion
           | call_experssion 
          ;

simple_expression: '(' experssion ')' 
                    | IDENTIFIER '[' experssion ']'
                    | pointer_experssion
                    | literal 
                    | IDENTIFIER 
                    ;

experssion_list: experssion | experssion_list ',' experssion ;

/* For option to call func with assign, for example: x = call foo(a,b) */
call_experssion:   CALL IDENTIFIER '('  ')' 
                 | CALL IDENTIFIER '(' experssion_list ')';


pointer_experssion:   '*' simple_expression  // we change IDENTIFIER to simple_expression to support : &y[5]
                    | '&' simple_expression  // we change IDENTIFIER to simple_expression to support : &y[5]
                    ; 

%%

/* For AST tree: */


node* mknode(char *token, node *left, node *right) {
    node *newnode = (node*)malloc(sizeof(node));
    char *newstr = (char*)malloc(strlen(token) + 1);
    strcpy(newstr, token);
    newnode->left = left;
    newnode->right = right;
    newnode->token = newstr;
    return newnode;
}

void printtree(node *tree)
{
    if (tree == NULL)
        return;
    if ( tree->left || tree->right)
        printf("(");               // פותח סוגריים לפני הצגת התוכן
    printf("%s", tree->token);  // מדפיס את הטוקן עצמו
    
    if (tree->left) {
        printf(" ");            // רווח קטן לפני מעבר לתת-עץ שמאלי
        printtree(tree->left);
    }

    if (tree->right) {
        printf(" ");            // רווח קטן לפני מעבר לתת-עץ ימני
        printtree(tree->right);
    }
    if (tree->left || tree->right)
        printf(")");                // סוגר סוגריים אחרי סיום הבנים
}

int yyerror(char *s) {
    printf("\nSyntax error at line %d: %s got %s\n", yylineno, s, yytext);
    return 0;
}

int main()
{
    printf("Starting parsing...\n");
    if(yyparse() == 0) {
        printf("Parse successful\n");
        if ( root != NULL ){
            printf("\nAST:\n");
            printtree(root);
            printf("\n");
    }   else {
        printf("No AST\n");
        }
    } else {    
    printf("Parsing FAILED.\n");
    }
    
    return 0;
    
}



