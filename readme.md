how to compile :

lex *scanner.l
yacc -d *parser.y
cc lex.yy.c y.tab.c symbol_table.c semantic.c -o compiler  -lfl

./compiler < input.txt

גיל אברהם : 318180213
שמעון ברוך 315385955