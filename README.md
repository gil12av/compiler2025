yacc -d parser.y
lex scanner.l
gcc -o compilerTest lex.yy.c y.tab.c -lfl
./compilerTest < input.txt
