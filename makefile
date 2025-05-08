CFLAGS = -std=c11 -Wall

all: parser

parser: lex.yy.c y.tab.c symbol_table.o semantic.o
	$(CC) $(CFLAGS) $^ -o $@ -lfl

y.tab.c y.tab.h: parser.y
	bison -d parser.y

lex.yy.c: scanner.l
	flex scanner.l

symbol_table.o: symbol_table.c symbol_table.h
	$(CC) $(CFLAGS) -c $<

semantics.o: semantic.c semantic.h symbol_table.h parser.y
	$(CC) $(CFLAGS) -c semantic.c

clean:
	rm -f lex.yy.c y.tab.* *.o parser
