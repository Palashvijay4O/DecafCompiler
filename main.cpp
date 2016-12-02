#include "AST.h"
#include <iostream>
#include <cstdio>
#include <fstream>
using namespace std;

ASTProgram * start;
ofstream outputFile;
extern "C" FILE *yyin;
extern "C" int yyparse();
extern "C" int yylex();

int main(int argv, char** argc) {
    // open a file handle to a particular file:
    FILE *myfile = fopen(argc[1], "r");
    // make sure it's valid:
    if (!myfile) {
        cout << "I can't open test_program!" << endl;
        return -1;
    }
    yyin = myfile;
    //outputFile.open ("XML_visitor.txt");
    do {
        yyparse();
    } while (!feof(yyin));
    //cout << "Success" << endl;

    CodeGenVisitor * foo = new CodeGenVisitor(start);
    foo->codeGen();
    //outputFile.close();
}
