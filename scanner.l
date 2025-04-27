%{

#include "y.tab.h"
#include <stdio.h>
#include <stdlib.h>
extern int yylineno;
/* For debug only 
int yywrap(){
  return 1;
} */
%}

%%

 /* KeyWords */
bool    { return BOOL; }
char	{ return CHAR; }
int     { return INT; }
real    { return REAL; }
string  { return STRING; }
int\*    { return INT_PTR; }
char\*   { return CHAR_PTR; }
real\*   { return REAL_PTR; }	
if      { return IF; }
elif    { return ELIF; }
else    { return ELSE; }
while   { return WHILE; }
for     { return FOR; }	
type      { return TYPE; }
var	      { return VAR; }
par[0-9]+ { yylval.string = strdup(yytext); return PAR; }	
return  { return RETURN_KEYWORD; }
null    { return NULL_KEYWORD; }
do      { return DO; }
returns { return RETURNS; }	
begin   { return BEGIN_KEYWORD; }
end     { return END_KEYWORD; }
def     { return DEF; }
call    { return CALL; }	
and     { return AND; }
not     { return NOT; }
or      { return OR; }
		
 /* Operator Lexemes */

"/"		{ yylval.string = strdup(yytext); return yytext[0]; }
"="		{ yylval.string = strdup(yytext); return yytext[0]; }
">" 	{ yylval.string = strdup(yytext); return yytext[0]; }
"<"		{ yylval.string = strdup(yytext); return yytext[0]; }
"-"		{ yylval.string = strdup(yytext); return yytext[0]; }
"+"		{ yylval.string = strdup(yytext); return yytext[0]; }
"*"		{ yylval.string = strdup(yytext); return yytext[0]; }
"&"		{ yylval.string = strdup(yytext); return yytext[0]; }
	
 /* Compare */
"==" 	 { yylval.string = strdup(yytext); return DOUBLE_EQUAL; }
"!=" 	 { return NOT_EQUAL; }
">=" 	 { return GREATER_EQUAL; }
"<=" 	 { return LESS_EQUAL; }
	
 /* Literal Lexemes */
TRUE 	 { return TRUE_LITERAL; }
FALSE  { return FALSE_LITERAL; }

"\""[^"]*"\""                           { yylval.string = strdup(yytext); return STRING_LITERAL; }
\'[.]\'	                                { yylval.string = strdup(yytext); return CHAR_LITERAL; }
[1-9][0-9]*                             { yylval.string = strdup(yytext); return INT_LITERAL; }
0                                       { yylval.string = strdup(yytext); return INT_LITERAL; }
[-+]?[0-9]*\.[0-9]*([eE]?[-+]?[0-9]+)?  { yylval.string = strdup(yytext); return REAL_LITERAL; }
                     
 /* Identifier */
[a-zA-Z][a-zA-Z0-9_]*   { yylval.string = strdup(yytext); return IDENTIFIER; }

 /* Other lexams */
";"		{ yylval.string = strdup(yytext); return yytext[0]; }
","	 	{ yylval.string = strdup(yytext); return yytext[0]; }
"("	 	{ yylval.string = strdup(yytext); return yytext[0]; }
")"		{ yylval.string = strdup(yytext); return yytext[0]; }
"["	 	{ yylval.string = strdup(yytext); return yytext[0]; }
"]"	 	{ yylval.string = strdup(yytext); return yytext[0]; }
":"	 	{ yylval.string = strdup(yytext); return yytext[0]; }
"|"	 	{ yylval.string = strdup(yytext); return yytext[0]; }
\n    { yylineno++; }

%%	
	
