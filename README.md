yacc -d parser.y // to update parser
lex scanner.l // to update scanner
gcc -o compilerTest lex.yy.c y.tab.c -lfl // compile both files
./compilerTest < input.txt // run the file on input file
