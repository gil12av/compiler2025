%{
#include <stdio.h>
#include <stdlib.h>
int yylex(void);
int yyerror(char *s);
int yydebug = 1;
extern int yylineno; // בשביל לעקוב אחר מספר שורה
extern char *yytext;
%}

typedef struct ASTnode {
    char *node_type;
    char *value;
    struct ASTnode *left;
    struct ASTnode *right;
    struct ASTnode *next;
} ASTnode;

%union {
    char *string;
    struct ASTnode* node;
}

ASTnode *create_node(char *type, char *value, ASTnode *left, ASTnode *right, ASTnode *next);
void print_ast(ASTnode *node, int depth);

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

%type <node> program function_list function return_value parameter_list
%type <node> parameter type declaration_list declaration variable_list variable1
%type <node> literal code_block inner_block optional_function_list optional_var
%type <node> statement_list statement lvalue simple_statement return_statement
%type <node> call_statement if_statement elif_list elif_blocks elif_block else_block
%type <node> single_block_or_block while_statement do_while_statement for_statement
%type <node> experssion simple_expression experssion_list pointer_experssion


%start program

%%

program: function_list { $$ = $1; }
        ;

function_list:  function                { $$ = $1; } 
              | function_list function  { $$ = create_node("CODE", NULL, $1, $2, NULL); }
              ;


function: DEF IDENTIFIER '(' parameter_list ')' ':' return_value code_block 
            {
                ASTnode *pars_node = create_node("PARS", NULL, $4, NULL, NULL);
                ASTnode *ret_node = create_node("RET", $7 ? $7->value: "NONE", NULL, NULL, NULL);
                ASTnode *func_node = create_node("FUNC", NULL, $2, pars_node, ret_node, $8);
                $$ = func_node;
            }
            ;

return_value :  /*empty*/          { $$ = NULL; } 
               | RETURNS type      { $$ = create_node("TYPE", $2, NULL, NULL, NULL); }
               ;

// about ast: if there just 1 parameter, we return PARS and if theres more than 1 we create node PARS
parameter_list: /*empty*/                         { $$ = NULL; }
                | parameter                       { $$ = $1; }
                | parameter_list ';' parameter    { $$ = create_node("PARS", NULL, $1, $3, NULL); }
                ;

// about ast: we want to print example: (par1 INT x) --> so we use buffer.
parameter: PAR type ':' IDENTIFIER 
            {
                char buffer[100];
                snprintf(buffer, sizeof(buffer), "%s %s", $2, $4);
                $$ = create_node($1, strdup(buffer), NULL, NULL, NULL)
            }
           ;

// about ast: each of types are return from scanner to PAR or return_value.
type:  INT       { $$ = $1; }
     | REAL      { $$ = $1; }
     | CHAR      { $$ = $1; }
     | STRING    { $$ = $1; }
     | BOOL      { $$ = $1; }
     | INT_PTR   { $$ = $1; }
     | CHAR_PTR  { $$ = $1; }
     | REAL_PTR  { $$ = $1; }
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


ASTnode *create_node(char *type, char *value, ASTnode *left, ASTnode *right, ASTnode *next) {
    ASTnode *node = (ASTnode*)malloc(sizeof(ASTnode));
    node-> node_type = strdup(type);
    node-> value = value ? strdup(value): NULL;
    node->left = left;
    node->right = right;
    node->next = next;
    return node;
}

void print_ast(ASTnode *node, int depth) {
    if (!node) return;
    for(int i = 0; i < depth; i++) printf("  ");
    printf("Node(%s", node-> node_type);
    if(node->value) printf(", value=%s", node->value);
    printf(")\n");
    print_ast(node->left, depth + 1);
    print_ast(node->right, depth + 1);
    print_ast(node->next, depth);
}

int yyerror(char *s) {
    printf("\nSyntax error at line %d: %s got %s\n", yylineno, s, yytext);
    return 0;
}

int main()
{
    yydebug = 1;
    printf("Starting parsing...\n");
    ASTnode* root = NULL;
    if (yyparse(&root) == 0){
        printf("\nAST:\n");
        print_ast(root, 0);
    } else {
        printf("Parsing FAILED.\n");
    }
    return 0;
    
}

