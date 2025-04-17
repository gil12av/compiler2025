%{
#include <stdio.h>
#include <stdlib.h>
int yylex(void);
int yyerror(char *s);
extern int yylineno; // בשביל לעקוב אחר מספר שורה
%}

%token BOOL CHAR INT REAL STRING
%token INT_PTR CHAR_PTR REAL_PTR
%token IF ELIF ELSE WHILE FOR
%token TYPE VAR PAR RETURN_KEYWORD NULL_KEYWORD DO RETURNS
%token BEGIN_KEYWORD END_KEYWORD DEF CALL
%token AND OR NOT
%token TRUE_LITERAL FALSE_LITERAL
%token HEX_LITERAL INT_LITERAL REAL_LITERAL STRING_LITERAL CHAR_LITERAL
%token IDENTIFIER
%token DOUBLE_EQUAL NOT_EQUAL GREATER_EQUAL LESS_EQUAL

%start program

%%

program: function_list;

function_list: function | function_list function ; 

function: DEF IDENTIFIER '(' parameter_list ')' ':' RETURNS type 
          VAR declaration_list
          BEGIN_KEYWORD body END_KEYWORD ;

parameter_list: parameter | parameter_list ';' parameter ;

parameter: PAR type ':' IDENTIFIER ;

type: INT | REAL | CHAR | STRING | BOOL | INT_PTR | CHAR_PTR | REAL_PTR ;

declaration_list: declaration | declaration_list declaration ;

declaration: TYPE type ':' variable_list ';' ;

variable_list: variable1 | variable_list ',' variable1 ;

variable1: IDENTIFIER | IDENTIFIER ':' literal ;

literal: INT_LITERAL | REAL_LITERAL | CHAR_LITERAL | HEX_LITERAL
        | STRING_LITERAL | TRUE_LITERAL | FALSE_LITERAL | NULL_KEYWORD ; 
 
 body: ';' ;

// ,body 




//כללי גזירה

%%

int yyerror(char *s) {
    printf("Syntax error at line %d: %s\n", yylineno, s);
    return 0;
}

int main()
{
    printf("Starting parsing...\n");
    int result = yyparse();
    
    if (result == 0) {
        printf("Parsing completed successfully!\n");
    } else {
        printf("Parsing failed.\n");
    }  

    return result;
    
}

