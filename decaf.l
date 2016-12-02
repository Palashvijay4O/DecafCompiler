%option noyywrap
%option nounput
%option yylineno
%{
#include <iostream>
using namespace std;
#define YY_DECL extern "C" int yylex()

#include "AST.h"

#include "decaf.tab.h"
char temp[100];
char * translateString(char *, int);

%}

COMMENT  "//".*
COMMENTS  "/*"(.|\n)*"*/"

CHAR ([\40-\176]{-}['"\\])|\\n|\\t|\\\\|\\\"|\\\'
%%


{COMMENT}   {
                
            }

{COMMENTS}  {
                
            }
[ \t\n]                 ;

"class"                 { return CLASS; }

"Program"               { return PROGRAM; }

"void"                  { return VOID; }

"true"                  { yylval.bval = true; return BOOLEAN; }

"false"                 { yylval.bval = false; return BOOLEAN; }

"int"                     { return INT_DECLARATION; }

"boolean"                 { return BOOLEAN_DECLARATION; }

"if"                    { return IF; }

"for"                   { return FOR; }

"else"                  { return ELSE; }

"return"                { return RETURN; }

"break"                 { return BREAK; }

"continue"              { return CONTINUE; }

"callout"               { return CALLOUT; }

[a-zA-Z_][a-zA-Z0-9_]*  { yylval.sval = strdup(yytext); return ID; }

"+"                     {return ADD; }
"-"                     {return SUB; }
"*"                     {return MUL; }
"/"                     {return DIV; }
"%"                     {return MOD; }
                                
"<"                     {return SMALLER; }
">"                     {return GREATER; }
"<="                    {return ESMALLER; }
">="                    {return EGREATER; }
"=="                    {return EQUALEQUAL; }
"!="                    {return NOTEQUAL; }

"="                     {return EQUAL; }
"+="					{return PLUS_EQUAL; }
"-="               		{return MINUS_EQUAL; }

"&&"                    {return AND; }
"||"                    {return OR; }

"("|")"|"{"|"}"|"["|"]"|","|"!"|";"     { return yytext[0]; }

\"{CHAR}*\"              { yylval.sval = translateString(yytext + 1, strlen(yytext) - 2); return STRING; }

\'{CHAR}\'              { yylval.cval = yytext[1]; return CHARACTER; }

0x[0-9a-fA-F]+          { yylval.ival = atoi(yytext); return INT; }

[0-9]+                  { yylval.ival = atoi(yytext); return INT; }

.                       {}
%%


char * translateString(char * string, int size) {
	char * ch = string;
	char * newString = (char *) malloc(size + 1);
	char * temp = newString;
	int i;
	for (i = 0; i < size; i++) {
		switch (*ch) {
			case '\\':
				i++;
				ch++;
				switch (*ch) {
					case 'a':
						*temp = '\a';
						break;
					case 'b':
						*temp = '\b';
						break;
					case 'f':
						*temp = '\f';
						break;
					case 'n':
						*temp = '\n';
						break;
					case 'r':
						*temp = '\r';
						break;
					case 't':
						*temp = '\t';
						break;
					case 'v':
						*temp = '\v';
						break;
					case '\\':
						*temp = '\\';
						break;
					case '\'':
						*temp = '\'';
						break;
					case '\"':
						*temp = '\"';
						break;
					case '?':
						*temp = '\?';
						break;
					case '0':
					case '1':
					case '2':
					case '3':
					case '4':
					case '5':
					case '6':
					case '7':
						//TODO: Fill
						break;
					case 'x':
						//TODO: Fill
						break;
					default:
						break;
				}
				break;
			default: 
				*temp = *ch;
				break;
		}
		ch++;
		temp++;
	}
	*temp = '\0';
	return newString;
}
