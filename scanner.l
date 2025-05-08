%{
#include <stdio.h>
#include <stdlib.h>
#include "y.tab.h"
extern int yylineno;
%}

/* ---------- קיצורי‑REGEX ---------- */
DIGIT      [0-9]
HEXDIGIT   [0-9A-Fa-f]
ID         [A-Za-z][A-Za-z0-9_]*
WS         [ \t\r]+
/* ---------------------------------- */

%%

{WS}                     {/* skip */}
\n                       { ++yylineno; }

/* block comment – #-> … <-# */
#->([^<]|\n)*<-#         { for(int i=0;i<yyleng;++i) if(yytext[i]=='\n') ++yylineno; }

/* pointer keywords – MUST precede base */
"int\\*"                 { return INT_PTR; }
"char\\*"                { return CHAR_PTR; }
"real\\*"                { return REAL_PTR; }

/* base keywords */
bool      { return BOOL; }
char      { return CHAR; }
int       { return INT; }
real      { return REAL; }
string    { return STRING; }

if        { return IF; }
elif      { return ELIF; }
else      { return ELSE; }
while     { return WHILE; }
for       { return FOR; }

type      { return TYPE; }
var       { return VAR; }
par[0-9]+ { yylval.string=strdup(yytext); return PAR; }

return    { return RETURN_KEYWORD; }
null      { return NULL_KEYWORD; }
do        { return DO; }
returns   { return RETURNS; }
begin     { return BEGIN_KEYWORD; }
end       { return END_KEYWORD; }
def       { return DEF; }
call      { return CALL; }

and       { return AND; }
or        { return OR; }
not       { return NOT; }

/* operators & delimiters */
"=="      { return DOUBLE_EQUAL; }
"!="      { return NOT_EQUAL; }
">="      { return GREATER_EQUAL; }
"<="      { return LESS_EQUAL; }
"/"|"="|">"|"<"|"-"|"+"|"*"|"&" { return yytext[0]; }
"\\|"     { return '|'; }             /* FIX: מחזירים תו '|' כדי להתאים לכלל grammar */

/* literals */
0[xX]{HEXDIGIT}+                        { yylval.string=strdup(yytext); return HEX_LITERAL; }
{DIGIT}+                                { yylval.string=strdup(yytext); return INT_LITERAL; }
[-+]?{DIGIT}*\\.{DIGIT}+([eE][-+]?{DIGIT}+)? { yylval.string=strdup(yytext); return REAL_LITERAL; }
\"[^\"\n]*\"                            { yylval.string=strdup(yytext); return STRING_LITERAL; }
\'.\'                                    { yylval.string=strdup(yytext); return CHAR_LITERAL; }
[Tt][Rr][Uu][Ee]                        { return TRUE_LITERAL; }
[Ff][Aa][Ll][Ss][Ee]                    { return FALSE_LITERAL; }

/* identifier */
{ID}       { yylval.string=strdup(yytext); return IDENTIFIER; }

/* unknown */
.          { fprintf(stderr,"Unknown char '%c' line %d\n",yytext[0],yylineno); exit(1); }

<<EOF>>    { return 0; }
%%
