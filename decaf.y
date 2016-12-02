%{
#include <cstdio>
#include <iostream>
#include <fstream>
#include <cstdarg>
#include "AST.h"
using namespace std;

extern "C" int yylex();
extern int yylineno;
extern "C" int yyparse();
extern "C" FILE *yyin;
extern ASTProgram * start;

int flag_check = 0;
ofstream out;

void yyerror(const char *s);
#define YYSTYPE_IS_DECLARED
%}


%token CLASS PROGRAM VOID IF FOR ELSE RETURN BREAK CONTINUE CALLOUT INT_DECLARATION BOOLEAN_DECLARATION ID STRING  PLUS_EQUAL MINUS_EQUAL ADD SUB DIV MUL MOD EQUAL SMALLER GREATER ESMALLER EGREATER EQUALEQUAL NOTEQUAL AND OR

%token <ival> INT
%token <bval> BOOLEAN
%token <cval> CHARACTER
%type <sval> ID
%type <sval> STRING

%type <program> program
%type <field_declarations> field_declarations
%type <field_declaration> field_declaration
%type <method_declarations> method_declarations
%type <method_declaration> method_declaration
%type <id_comma> id_comma
%type <arguments> arguments
%type <block> block
%type <literal> literal
%type <var_declarations> var_declarations
%type <var_declaration> var_declaration
%type <statements> statements
%type <statement> statement
%type <expr> expr
%type <id_commas> id_commas
%type <expr_comma> expr_comma
%type <location> location
%type <assign_op> assign_op
%type <method_call> method_call
%type <callout_arg_comma> callout_arg_comma


%left OR
%left AND
%nonassoc SMALLER GREATER ESMALLER EGREATER
%left EQUALEQUAL NOTEQUAL

%left ADD SUB
%left MUL DIV MOD

%precedence '!' USUB
%%

program:
    CLASS PROGRAM '{' field_declarations '}' { $$ = new ASTProgram($4, NULL); start = $$; }
    | CLASS PROGRAM '{' method_declarations '}' { $$ = new ASTProgram(NULL, $4); start = $$; }
    | CLASS PROGRAM '{' field_declarations method_declarations '}' { $$ = new ASTProgram($4, $5); start = $$; }
    | CLASS PROGRAM '{' '}' { $$ = new ASTProgram(NULL, NULL); }
    ;


field_declarations:
    field_declarations field_declaration { $1 -> push_back($2); $$ = $1; } 
    | field_declaration { $$ = new vector < ASTFieldDeclaration * >(); $$ -> push_back($1); }
    ;
    
field_declaration:
    INT_DECLARATION id_comma ';' {$$ = new ASTFieldDeclaration(datatype::int_type, $2); }
    | BOOLEAN_DECLARATION id_comma ';' { $$ = new ASTFieldDeclaration(datatype::bool_type, $2); }
    ;

id_comma:
    id_comma ',' ID { $1 -> push_back(new ASTVarIdentifier($3)); $$ = $1; }
    | id_comma ',' ID '[' INT ']' { $1 -> push_back(new ASTArrayIdentifier($3, $5)); $$ = $1; }
    | ID { $$ = new vector < ASTIdComma * >(); $$ -> push_back(new ASTVarIdentifier($1)); }
    | ID '[' INT ']' { $$ = new vector < ASTIdComma * >(); $$ -> push_back(new ASTArrayIdentifier($1, $3)); }
    ;
    

method_declarations:
    method_declarations method_declaration  { $1->push_back($2); $$ = $1; }
    | method_declaration { $$ = new vector<ASTMethodDeclaration*>(); $$->push_back($1); }
    ;

method_declaration:
    INT_DECLARATION ID '(' arguments ')' block { $$ = new ASTMethodDeclaration(datatype::int_type, $2, $4, $6); }
    | BOOLEAN_DECLARATION ID '(' arguments ')' block { $$ = new ASTMethodDeclaration(datatype::bool_type, $2, $4, $6); }
    | VOID ID '(' arguments ')' block { $$ = new ASTMethodDeclaration(datatype::void_type, $2, $4, $6); }
    ;

arguments:
    arguments ',' INT_DECLARATION ID { $1->push_back(new ASTArgument(datatype::int_type, $4)); $$ = $1; }
    | arguments ',' BOOLEAN_DECLARATION ID { $1->push_back(new ASTArgument(datatype::bool_type, $4)); $$ = $1; }
    | INT_DECLARATION ID { $$ = new vector<ASTArgument*>(); $$->push_back(new ASTArgument(datatype::int_type, $2)); }
    | BOOLEAN_DECLARATION ID { $$ = new vector<ASTArgument*>(); $$->push_back(new ASTArgument(datatype::bool_type, $2)); }
    | empty  { $$ = new vector<ASTArgument*>(); }
    ;
    
block:
    '{' var_declarations statements '}' { $$ = new ASTBlock($2, $3); }
    | '{' statements '}'  { $$ = new ASTBlock(NULL, $2); }
    | '{' var_declarations '}'  { $$ = new ASTBlock($2, NULL); }
    | '{' '}'  { $$ = new ASTBlock(NULL, NULL); }
    ;
    
var_declarations:
    var_declarations var_declaration  { $1->push_back($2); $$ = $1;}
    | var_declaration { $$ = new vector<ASTVarDeclaration*>(); $$->push_back($1); }
    ;
    
var_declaration:
    INT_DECLARATION id_commas ';' { $$ = new ASTVarDeclaration(datatype::int_type, $2); }
    | BOOLEAN_DECLARATION id_commas ';' { $$ = new ASTVarDeclaration(datatype::bool_type, $2); }
    ;
    
id_commas:
    id_commas ',' ID  { $1->push_back(new ASTIdCommas($3)); $$ = $1; }
    | ID { $$ = new vector<ASTIdCommas*>(); $$->push_back(new ASTIdCommas($1)); }
    ;

statements:                         
    statements statement            { $1->push_back($2); $$ = $1; }
    | statement                     { $$ = new vector<ASTStatement*>(); $$->push_back($1); }
    ;
    
statement:
    location assign_op expr ';'  { $$ = new ASTAssignmentStatement($1, $2, $3); }
    | method_call ';'           { $$ = $1; }
    | IF '(' expr ')' block ELSE block { $$ = new ASTIfElseStatement($3, $5, $7); }
    |  IF '(' expr ')' block           { $$ = new ASTIfStatement($3, $5); }          
    | FOR ID EQUAL expr ',' expr block { $$ = new ASTForStatement($2, $4, $6, $7); }
    | RETURN expr ';'       { $$ = new ASTReturnStatement($2); }
    | RETURN ';'            { $$ = new ASTReturnStatement(NULL); }
    | BREAK ';'                 { $$ = new ASTBreakStatement(); }
    | CONTINUE ';'              {  $$ = new ASTContinueStatement(); }
    | block                     { $$ = new ASTBlockStatement($1); }
    ;
    

method_call:
    ID '(' expr_comma ')'   { $$ = new ASTNameMethodCall($1, $3); }
    | CALLOUT '(' STRING ')'        { $$ = new ASTCallout(std::string($3), NULL); }
    | CALLOUT '(' STRING ',' callout_arg_comma ')' { $$ = new ASTCallout(std::string($3), $5); }
    ;


expr_comma:
    empty      { $$ = new vector<ASTExpression*>(); } 
    | expr_comma ',' expr  { $1->push_back($3); $$ = $1; }
    | expr      { $$ = new vector<ASTExpression*>(); $$->push_back($1); }
    ;
     
expr:
    location              { $$ = $1; }
    | method_call         { $$ = $1; }
    | literal             { $$ = $1; } 
    | expr ADD expr       { $$ = new ASTBinaryExpression($1, arithematic_op::plus_op, $3); }
    | expr SUB expr       { $$ = new ASTBinaryExpression($1, arithematic_op::minus_op, $3); }
    | expr MUL expr       { $$ = new ASTBinaryExpression($1, arithematic_op::multiply_op, $3); }
    | expr DIV expr       { $$ = new ASTBinaryExpression($1, arithematic_op::divide_op, $3); }
    | expr MOD expr       { $$ = new ASTBinaryExpression($1, arithematic_op::modulo_op, $3); }
    | expr SMALLER expr    { $$ = new ASTBinaryExpression($1, arithematic_op::less_than, $3); }
    | expr ESMALLER expr    { $$ = new ASTBinaryExpression($1, arithematic_op::less_than_equal, $3); }
    | expr GREATER expr     { $$ = new ASTBinaryExpression($1, arithematic_op::more_than, $3); }
    | expr EGREATER expr    { $$ = new ASTBinaryExpression($1, arithematic_op::more_than_equal, $3); }
    | expr EQUALEQUAL expr  { $$ = new ASTBinaryExpression($1, arithematic_op::equal_equal, $3); }
    | expr NOTEQUAL expr    { $$ = new ASTBinaryExpression($1, arithematic_op::not_equal, $3); }
    | expr AND expr         { $$ = new ASTBinaryExpression($1, arithematic_op::and_and, $3); }
    | expr OR expr          { $$ = new ASTBinaryExpression($1, arithematic_op::or_or, $3); }
    | SUB expr %prec USUB { $$ = new ASTUnaryExpression(arithematic_op::minus_op, $2); }
    | '!' expr            { $$ = new ASTUnaryExpression(arithematic_op::unot, $2); }
    | '(' expr ')'        { $$ = $2; }
    ;

location:
    ID                    { $$ = new ASTVarLocation($1); }
    | ID '[' expr ']'     {  $$ = new ASTArrayLocation($1, $3); }
    ;

callout_arg_comma:
    callout_arg_comma ',' expr     { $1->push_back(new ASTCalloutArgumentExpr($3)); $$ = $1; }
    | callout_arg_comma ',' STRING { $1->push_back(new ASTCalloutArgumentString(std::string($3))); $$ = $1; }
    | expr        { $$ = new vector<ASTCalloutArgument*>(); $$->push_back(new ASTCalloutArgumentExpr($1)); } 
    | STRING     { $$ = new vector<ASTCalloutArgument*>(); $$->push_back(new ASTCalloutArgumentString(std::string($1))); } 
    ;



literal:
    INT                 { $$ = new ASTIntLiteral($1); }
    | CHARACTER         { $$ = new ASTCharLiteral($1); }
    | BOOLEAN           { $$ = new ASTBoolLiteral($1); }
    ;

assign_op:
    EQUAL               { $$ = new ASTAssignOperator(assign_op::equal); }
    | PLUS_EQUAL         { $$ = new ASTAssignOperator(assign_op::plus_equal); }
    | MINUS_EQUAL         { $$ = new ASTAssignOperator(assign_op::minus_equal); }
    ;
    

empty:

%%

void yyerror(const char *s) {
    std::cerr << "Parse Error on Line : " << yylineno << std::endl << "Message : " << s << std::endl;
    exit(0);
}
