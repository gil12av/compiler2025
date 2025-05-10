# Makefile
CC      := gcc
CFLAGS  := -Wall -g -I.

# כללי Bison/Flex
YACC    := bison
YFLAGS  := -d
LEX     := flex

# שמות קבצים
PARSER_SRC    := parser.y
LEXER_SRC     := scanner.l
Y_TAB_C       := y.tab.c
Y_TAB_H       := y.tab.h
LEXER_C       := lex.yy.c
OBJS          := parser.o lex.yy.o semantic.o symbol_table.o

.PHONY: all clean

all: parser

# הקומבינציה הסופית
parser: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) -lfl

# Parser.o: יוצרים קודם y.tab.c/h ואז מקמפלים
parser.o: $(PARSER_SRC)
	$(YACC) $(YFLAGS) $<
	$(CC) $(CFLAGS) -c $(Y_TAB_C) -o $@

# Scanner.o: יוצרים קודם lex.yy.c (דורש y.tab.h), ואז מקמפלים
lex.yy.o: $(LEXER_SRC) $(Y_TAB_H)
	$(LEX) $<
	$(CC) $(CFLAGS) -c $(LEXER_C) -o $@

# Semantic & SymbolTable
semantic.o: semantic.c semantic.h $(Y_TAB_H)
	$(CC) $(CFLAGS) -c semantic.c -o $@

symbol_table.o: symbol_table.c symbol_table.h
	$(CC) $(CFLAGS) -c symbol_table.c -o $@

clean:
	rm -f parser $(OBJS) $(Y_TAB_C) $(Y_TAB_H) $(LEXER_C)
