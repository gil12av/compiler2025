%{
#include <stdio.h>
#include <stdlib.h>
int yylex(void);
int yyerror(char *s);
int yydebug = 1;
extern int yylineno; // בשביל לעקוב אחר מספר שורה
extern char *yytext;
%}

%left '+' '-'
%left '*' '/'
%left '<' '>' DOUBLE_EQUAL NOT_EQUAL GREATER_EQUAL LESS_EQUAL

%left OR
%left AND
%right NOT


%token BOOL CHAR INT REAL STRING
%token INT_PTR CHAR_PTR REAL_PTR
%token IF ELIF ELSE WHILE FOR
%token TYPE VAR PAR RETURN_KEYWORD NULL_KEYWORD DO RETURNS
%token BEGIN_KEYWORD END_KEYWORD DEF CALL
%token TRUE_LITERAL FALSE_LITERAL
%token HEX_LITERAL INT_LITERAL REAL_LITERAL STRING_LITERAL CHAR_LITERAL
%token IDENTIFIER
/* %token DOUBLE_EQUAL NOT_EQUAL GREATER_EQUAL LESS_EQUAL */

%start program

%%

program: function_list;

function_list: function | function_list function ; 

function: DEF IDENTIFIER '(' parameter_list ')' ':' return_value
          code_block ;

return_value :  | RETURNS type ;

parameter_list: | parameter | parameter_list ';' parameter ;

parameter: PAR type ':' IDENTIFIER ;

type: INT | REAL | CHAR | STRING | BOOL | INT_PTR | CHAR_PTR | REAL_PTR ;

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
int yyerror(char *s) {
    printf("\nSyntax error at line %d: %s got %s\n", yylineno, s, yytext);
    return 0;
}

int main()
{
    yydebug = 1;
    printf("Starting parsing...\n");
    int result = yyparse();
    if (result == 0) {
        printf("\nParsing completed successfully!\n");
    } else {
        printf("\nParsing failed.\n");
    }  

    return result;
    
}

