step 1: 
yacc -d parser.y  --->  to update parser

step 2:
lex scanner.l ---> to update scanner

step 3:
gcc -o compilerTest lex.yy.c y.tab.c -lfl ---> compile both files

step 4:
./compilerTest < input.txt ---> run the file on input file



if you want do debug:
bison -d -v parser.y
flex scanner.l
gcc -g -DYYDEBUG parser.tab.c lex.yy.c -o compilerDebug

