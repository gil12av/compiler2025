%{

#include "y.tab.h"
#include <stdio.h>
#include <stdlib.h>

%}

%%
 /* KeyWords */
bool    { return BOOL; }
char	{ return CHAR; }
int     { return INT; }
real    { return REAL; }
string  { return STRING; }
int*    { return INT_PTR; }
char*   { return CHAR_PTR; }
real*   { return REAL_PTR; }	
if      { return IF; }
elif    { return ELIF; }
else    { return ELSE; }
while   { return WHILE; }
for     { return FOR; }	
type      { return TYPE; }
var	      { return VAR; }
par[0-9]+ { return PAR; }	
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

"/"		{ return yytext[0]; }
"="		{ return yytext[0]; }
">" 	{ return yytext[0]; }
"<"		{ return yytext[0]; }
"-"		{ return yytext[0]; }
"+"		{ return yytext[0]; }
"*"		{ return yytext[0]; }
"&"		{ return yytext[0]; }
	
 /* Compare */
"==" 	 { return DOUBLE_EQUAL; }
"!=" 	 { return NOT_EQUAL; }
">=" 	 { return GREATER_EQUAL; }
"<=" 	 { return LESS_EQUAL; }
	
 /* Literal Lexemes */
TRUE 	 { return TRUE_LITERAL; }
FALSE  { return FALSE_LITERAL; }
	
\'[ -~]\'	 { return CHAR_LITERAL; }
	
0[xX][0-9a-fA-F]+	 { return HEX_LITERAL; }
[1-9][0-9]*	 	 	 { return INT_LITERAL; }
0					 { return INT_LITERAL; }


  /* String */
\"([^\"\n])*\"        { return STRING_LITERAL; }
	
 /* Identifier */
[a-zA-Z][a-zA-Z0-9_]* { return IDENTIFIER; }

 /* Other lexams */
";"		{ return yytext[0]; }
","	 	{ return yytext[0]; }
"("	 	{ return yytext[0]; }
")"		{ return yytext[0]; }
"["	 	{ return yytext[0]; }
"]"	 	{ return yytext[0]; }
":"	 	{ return yytext[0]; }
"|"	 	{ return '|'; }

%%	
	
