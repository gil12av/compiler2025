%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int yylex(void);
int yyerror(char *s);
int yydebug = 1;
extern int yylineno; 
extern char *yytext;

typedef struct node {
    char *token;
    struct node *left;
    struct node *right;
} node;

node* mknode(char *token, node *left, node *right);
void printtree(node* tree, int level);

// global root this root node is the "start" node .
node* root = NULL;


%}

/*the union tell yacc about the types */

%union {
    char *string;
    struct node* node;
}

/*the left , right is the key words are for the order of the experssion */


%left '+' '-'
%left '*' '/'
%left '<' '>' DOUBLE_EQUAL NOT_EQUAL GREATER_EQUAL LESS_EQUAL

%left OR
%left AND
%right NOT

%token <string> BOOL CHAR INT REAL STRING COMMENT
%token <string> INT_PTR CHAR_PTR REAL_PTR
%token <string> IF ELIF ELSE WHILE FOR
%token <string> TYPE VAR PAR RETURN_KEYWORD NULL_KEYWORD DO RETURNS
%token <string> BEGIN_KEYWORD END_KEYWORD DEF CALL
%token <string> TRUE_LITERAL FALSE_LITERAL
%token <string> HEX_LITERAL INT_LITERAL REAL_LITERAL STRING_LITERAL CHAR_LITERAL
%token <string> IDENTIFIER


%type <node> program function_list function return_value parameter_list parameter  Comments
%type <node> type declaration_list declaration variable_list variable1
%type <node> literal code_block inner_block optional_function_list optional_var
%type <node> statement_list statement lvalue simple_statement return_statement
%type <node> call_statement if_statement elif_list elif_blocks elif_block else_block
%type <node> single_block_or_block while_statement do_while_statement for_statement
%type <node> experssion simple_expression experssion_list pointer_experssion call_experssion



%start program

%%

program: function_list  
        { 
            $$ = mknode("CODE", $1, NULL); 
            root = $$;
        } 
        ;
/*
            program
                |
            function_list
*/
function_list:  function                    { $$ = $1; }
              | function_list function      
                { 
                    node* temp = $1;
                    while(temp->right != NULL)
                        temp = temp->right;
                    temp->right = $2;    
                    $$ = $1; 
                }
              ;

/*
option 1 :
            function_list
                |
            function
option 2 :
            function_list
                /   \
        function_list function


note : the while loop containing the "functions" for us to control the tree
*/
function: DEF IDENTIFIER '(' parameter_list ')' ':' return_value code_block 
            {
                node* nameNode = mknode($2, NULL, NULL);    // Function name
                node* parsNode = $4;                    // Pararmeter
                node* retNode = $7;                     // Return_value
                node* bodyNode = mknode("BODY", $8, NULL);  // Code_block 

                node* header = mknode("", nameNode, parsNode);
                node* parts = mknode("", retNode, bodyNode);
                $$ = mknode("FUNC",header, parts);
            } 
            ;

return_value :  /*empty*/       { $$ = mknode("RET NONE", NULL, NULL); }         
               | RETURNS type  
                { 
                    char returnValue[256];
                    sprintf(returnValue, "RET %s", $2->token);
                    $$ = mknode(returnValue, NULL, NULL);
                }    
               ;

// about ast: if there just 1 parameter, we return PARS and if theres more than 1 we create node PARS
parameter_list: /*empty*/                       { $$ = mknode("PARS NONE", NULL, NULL); }                      
                | parameter                     { $$ = mknode("PARS", $1, NULL); } 
                | parameter_list ';' parameter  
                   { 
                       node* temp = $1;
                       while(temp->right != NULL) 
                          temp = temp->right;
                       temp->right = $3;
                       $$ = $1;   
                   }  
                ;

// about ast: we want to print example: (par1 INT x) --> so we use buffer.
parameter: PAR type ':' IDENTIFIER  
           { 
                char buffer[256];
                sprintf(buffer, "%s %s %s", $1, $2->token, $4);
                $$ = mknode(buffer, NULL, NULL);
           }
           ;

// about ast: each of types are return from scanner to PAR or return_value.
type:  INT          { $$ = mknode("INT", NULL,NULL); }
     | REAL         { $$ = mknode("REAL", NULL,NULL); }
     | CHAR         { $$ = mknode("CHAR", NULL,NULL); }
     | STRING       { $$ = mknode("STRING", NULL,NULL); }
     | BOOL         { $$ = mknode("BOOL", NULL,NULL); }
     | INT_PTR      { $$ = mknode("INT_PTR", NULL,NULL); }
     | CHAR_PTR     { $$ = mknode("CHAR_PTR", NULL,NULL); }
     | REAL_PTR     { $$ = mknode("REAL_PTR", NULL,NULL); }
     ;
Comments: /*empty*/ { $$ = NULL; } 
        | COMMENT { $$ = NULL; };

declaration_list: Comments  declaration                  { $$ = $2; }
                 | declaration_list declaration   
                   { 
                       node* temp = $1;
                       while(temp->right != NULL) 
                          temp = temp->right;
                       temp->right = $2;
                       $$ = $1;    
                   }
                 ;

declaration: TYPE type ':' variable_list ';'  Comments
              {
                 node* typeNode = mknode($2->token, NULL, NULL); 
                 $$ = mknode("DECLERATION", typeNode, mknode($4->token,NULL,NULL));
              }
             ;



variable_list:  variable1     { $$ = $1;}
              | variable_list ',' variable1      
              { 
                  node* temp = $1;
                  while(temp->right != NULL) 
                    temp = temp->right;
                  temp->right = $3;
                  $$ = $1;  
              }
              ;

variable1:  IDENTIFIER       // regular variable
            { $$ = mknode($1, NULL, NULL); }   // AST: Create node with variable name.

          | IDENTIFIER ':' literal    // initialized var
            { 
                node* idNode = mknode($1, NULL, NULL);
                $$ = mknode("=", idNode ,$3);
            }    // AST: create node "="

          | IDENTIFIER '[' INT_LITERAL ']'     // array ( example: a[10] )
            { 
                char buffer[256];
                sprintf(buffer, "%s[%s]", $1, $3);
                $$ = mknode(buffer, NULL, NULL); 
            }  // AST: node (name to size)

          | IDENTIFIER '[' INT_LITERAL ']' ':' STRING_LITERAL   // array with value or init
            { 
               char buffer[256];
                sprintf(buffer, "%s[%s] = %s", $1, $3, $6);
                $$ = mknode(buffer, NULL, NULL); 
            }
          ;

literal:  INT_LITERAL       { $$ = mknode($1, NULL,NULL); }
        | REAL_LITERAL      { $$ = mknode($1, NULL,NULL); }
        | CHAR_LITERAL      { $$ = mknode($1, NULL,NULL); }
        | HEX_LITERAL       { $$ = mknode($1, NULL,NULL); }
        | STRING_LITERAL    { $$ = mknode($1, NULL,NULL); }
        | TRUE_LITERAL      { $$ = mknode($1, NULL,NULL); }
        | FALSE_LITERAL     { $$ = mknode($1, NULL,NULL); }
        | NULL_KEYWORD      { $$ = mknode($1, NULL,NULL); }
        ; 
 
code_block: optional_var BEGIN_KEYWORD Comments inner_block Comments END_KEYWORD 
            {   
                $$ = mknode("BLOCK", $1, $3);
            }
            ;

inner_block : optional_function_list statement_list
              {
                $$ = mknode("", $1, $2);
              }
              ;

optional_function_list:   /* empty */       { $$ = mknode("", NULL, NULL); } // We dont want segmentation fualt...
                        | function_list     { $$ = $1; }  // Return the tree that build on function_list
                        ;

optional_var :  /* empty */             { $$ = NULL; }
               | VAR declaration_list   { $$ = $2; }     //Return tree from decleration_list.
               ;

statement_list: { $$ = NULL; }|statement    { $$ = $1; }
               | statement statement_list   
               {
                  node* temp = $1;
                  while(temp->right != NULL)
                    temp = temp->right;
                  temp->right = $2;  
                  $$ = $1 ;
               }
               ;

// about AST; each of these statments are node that we build on previous rules, so we return it back and dont create new tree 
statement:   simple_statement      { $$ = $1; }
            | call_statement        { $$ = $1; }
            | if_statement          { $$ = $1; }
            | while_statement       { $$ = $1; }
            | do_while_statement    { $$ = $1; }
            | for_statement         { $$ = $1; }
            | code_block            { $$ = $1; }
            ;


/* This is the left side of the assignment... */
lvalue:   IDENTIFIER 
            { $$ = mknode($1, NULL, NULL); }
        | IDENTIFIER '[' experssion ']'  // for example of a[19] that makes us a problem
            { $$ = mknode("ARRAY_ACCESS", mknode($1, NULL, NULL), $3); } // AST: we want IDENTIFIER and ARRAY-INDEX
        | '*' IDENTIFIER               // For pointers, For example: *x
            { $$ = mknode("POINTER_ACCESS", mknode($2, NULL, NULL), NULL); } // AST: pointer approach
        | '&' IDENTIFIER               // For pointers, For example: *x
            { $$ = mknode("POINTER_ACCESS", mknode($2, NULL, NULL), NULL); } // AST: pointer approach
        ; 

simple_statement: lvalue '=' experssion ';'  Comments  { $$ = mknode("=", $1, $3); }
                 | return_statement             { $$ = $1; }
                 ;

return_statement: RETURN_KEYWORD experssion ';' { $$ = mknode("RET", $2, NULL); }
                 ;

/* For procedure only, for example: call foo(), for function is in line 300 */
call_statement :   CALL IDENTIFIER '('  ')' ';' // Procedure without parameter
                     { $$ = mknode("CALL", mknode($2, NULL, NULL), NULL); }
                 | CALL IDENTIFIER '(' experssion_list ')' ';'    // Procedure with parameter
                     { $$ = mknode("CALL", mknode($2, NULL, NULL), $4); }
                 ;

if_statement: IF experssion ':' single_block_or_block elif_list else_block 
            {
                node* if_node = mknode("IF", $2, $4);
                if ($5 != NULL)
                    if_node = mknode("IF-ELSE", if_node, $5);
                if ($6 != NULL)
                    if_node = mknode("IF_ELIF_ELSE", if_node, $6);
                $$ = if_node;
            }
            ;

elif_list:  /* empty */   {$$ = NULL;}  /*{ $$ = mknode("LINE330FIX", NULL, NULL); }*/ 
            | elif_blocks   { $$ = $1; }
            ;

elif_blocks:  elif_block                 { $$ = $1; }   // AST: if theres only one block - return it.
            | elif_blocks elif_block     
            {
                node* temp = $1;
                while(temp->right != NULL)
                   temp = temp->right;
                temp->right = $2;  
                $$ = $1 ;
            }
              // AST: if have more than 1 - create node to each block
            ;

elif_block: ELIF experssion ':' single_block_or_block  { $$ = mknode("ELIF", $2, $4); } 
            ;

else_block:  /* empty */         { $$ = NULL; }     /*{ $$ = mknode("NO_ELSE", NULL, NULL); } */
            | ELSE ':' single_block_or_block    { $$ = mknode("ELSE", $3, NULL); }
            ;

single_block_or_block:  simple_statement   { $$ = $1; } 
                       | code_block        { $$ = $1; } 
                       ;

while_statement: WHILE experssion ':' single_block_or_block  { $$ = mknode("WHILE", $2, $4); }
                 ;

// AST: we build DO ( from optional_var and statment_list ) and then connected it to DO_WHILE !
do_while_statement:  DO ':' optional_var BEGIN_KEYWORD statement_list END_KEYWORD WHILE  experssion ';' 
                     {
                        node* body = $3;
                        if (body != NULL) {
                            node* temp = body;
                            while ( temp->right != NULL)
                                temp = temp->right;
                            temp->right = $5;
                            body = $3;
                        } else {
                         body = $5;
                        } 
                        $$ = mknode("DO_WHILE", body, mknode($7, NULL, NULL));
                     }
                    ;

for_statement: FOR '(' IDENTIFIER '=' experssion ';' experssion ';' IDENTIFIER '=' experssion ')' ':' single_block_or_block 
                {
                    node* init = mknode("=", mknode($3, NULL, NULL), $5); 
                    node* cond = $7;
                    node* update = mknode("=", mknode($9, NULL, NULL), $11);
                    node* control = mknode("FOR_CTRL", init, mknode("COND_UPDATE", cond, update));
                    $$ = mknode("FOR",control, $14);
                }
              ;

experssion: simple_expression               { $$ = $1; } 
           | experssion '+' experssion      { $$ = mknode("+", $1, $3); } 
           | experssion '-' experssion      { $$ = mknode("-", $1, $3); } 
           | experssion '*' experssion      { $$ = mknode("*", $1, $3); } 
           | experssion '/' experssion      { $$ = mknode("/", $1, $3); } 
           | experssion '<' experssion      { $$ = mknode("<", $1, $3); } 
           | experssion '>' experssion      { $$ = mknode(">", $1, $3); } 
           | '|' experssion '|'             { $$ = mknode("SIZEOF", $2, NULL); } // For size of variable //
           | experssion DOUBLE_EQUAL experssion      { $$ = mknode("==", $1, $3); }
           | experssion NOT_EQUAL experssion         { $$ = mknode("!=", $1, $3); }
           | experssion GREATER_EQUAL experssion     { $$ = mknode(">=", $1, $3); }
           | experssion LESS_EQUAL experssion        { $$ = mknode("<=", $1, $3); }
           | experssion AND experssion               { $$ = mknode("AND", $1, $3); }
           | experssion OR experssion                { $$ = mknode("OR", $1, $3); }
           | NOT experssion                          { $$ = mknode("NOT", $2, NULL); }
           | call_experssion                         { $$ = $1; }
          ;

simple_expression: '(' experssion ')'                  { $$ = $2; }
                    | IDENTIFIER '[' experssion ']'    { $$ = mknode("ARRAY_ACCESS", mknode($1, NULL, NULL), $3); }
                    | pointer_experssion               { $$ = $1; }
                    | literal                          { $$ = $1; }
                    | IDENTIFIER                       { $$ = mknode($1, NULL, NULL); }
                    | IDENTIFIER '(' ')'                    { $$ = mknode("CALL", mknode($1, NULL, NULL), NULL); }
                    | IDENTIFIER '(' experssion_list ')'    { $$ = mknode("CALL", mknode($1, NULL, NULL), $3); }
                    ;

experssion_list:  experssion                          { $$ = $1; }
                | experssion_list ',' experssion      
                { 
                   node* temp = $1;
                   while(temp->right != NULL)
                     temp = temp->right;
                   temp->right = $3;  
                   $$ = $1 ;
                }
                ;

/* For option to call func with assign, for example: x = call foo(a,b) */
call_experssion:   CALL IDENTIFIER '('  ')'                 { $$ = mknode("CALL", mknode($2, NULL,NULL), NULL);}
                 | CALL IDENTIFIER '(' experssion_list ')' { $$ = mknode("CALL", mknode($2, NULL, NULL), $4); }
                 ;

pointer_experssion:   '*' simple_expression  // we change IDENTIFIER to simple_expression to support : *y[5]
                        { $$ = mknode("DEREF", $2, NULL); }
                    | '&' simple_expression  // we change IDENTIFIER to simple_expression to support : &y[5]
                        { $$ = mknode("ADDRESS", $2, NULL); }

                    ; 

%%

#include <stdio.h>

int yyerror(char *s) {
    printf("\nSyntax error at line %d: %s got %s\n", yylineno, s, yytext);
    return 0;
}


node* mknode(char *token, node *left, node *right) {
    node *newnode = (node*)malloc(sizeof(node));
        if(!newnode){
            printf("Memory Allocation Failed");
            exit(1);
        }
    if(token != NULL) {
         char *newstr = (char*)malloc(strlen(token) + 1);
         if(!newstr) {
            printf("Memory Allocation for token failed.");
            exit(1);
         }
         strcpy(newstr, token);
         newnode->token = newstr;
    }    else {
            newnode->token = NULL;
    }
   
    newnode->left = left;
    newnode->right = right;
    return newnode;
}

void printtree(node* tree, int level) {
    if (tree == NULL) return;

    int has_token = (tree->token != NULL && tree->token[0] != '\0');
    int is_leaf = (tree->left == NULL && tree->right == NULL);

    if (!has_token) {
        if (tree->left)  printtree(tree->left, level);
        if (tree->right) printtree(tree->right, level);
        return;
    }

    for (int i = 0; i < level; i++) printf("  ");

    if (is_leaf) {
        printf("(%s)\n", tree->token);
        return;
    }

    printf("(%s\n", tree->token);

    if (tree->left)  printtree(tree->left, level + 1);
    if (tree->right) printtree(tree->right, level + 1);

    for (int i = 0; i < level; i++) printf("  ");
    printf(")\n");
}






int main() {
    printf("Starting to parse input ..... \n");
    int result = yyparse();

    if (result == 0) {
        printf("\nYEAH! Parse Successful!\n");
        printf("Here is the AST tree:\n\n");
        if (root != NULL) {
            printtree(root, 0);
            printf("\n");
        } else {
            printf("No AST generated.\n");
        }
    } else {
        printf("Parse Failed.\n");
    }
    return result;
}



