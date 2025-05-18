how to compile :

lex *scanner.l
yacc -d *parser.y
cc lex.yy.c y.tab.c symbol_table.c semantic.c -o compiler  -lfl

./compiler < input.txt

גיל ושימי 
