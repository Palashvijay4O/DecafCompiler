#include <llvm/Analysis/Verifier.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/PassManager.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Assembly/PrintModulePass.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Pass.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/MathExtras.h>
#include <vector>
#include <string>
#include <fstream>
#include <iostream>
#include "SymbolTable.h"
#include "Visitor.h"

extern "C" ofstream outputFile;
#define TAB 1

using namespace std;

// Classes Declaration 
class ASTBlock;
class ASTProgram;
class ASTCallout;
class ASTLiteral;
class ASTIdComma;
class ASTIdCommas;
class ASTArgument;
class ASTLocation;
class ASTStatement;
class ASTMethodCall;
class ASTIntLiteral;
class ASTExpression;
class ASTCharLiteral;
class ASTBoolLiteral;
class ASTIfStatement;
class ASTVarLocation;
class ASTForStatement;
class ASTArrayLocation;
class ASTVarIdentifier;
class ASTNameMethodCall;
class ASTAssignOperator;
class ASTBreakStatement;
class ASTBlockStatement;
class ASTVarDeclaration;
class ASTUnaryExpression;
class ASTIfElseStatement;
class ASTReturnStatement;
class ASTArrayIdentifier;
class ASTCalloutArgument;
class ASTBinaryExpression;
class ASTFieldDeclaration;
class ASTMethodDeclaration;
class ASTContinueStatement;
class ASTAssignmentStatement;
class ASTCalloutArgumentExpr;
class ASTCalloutArgumentString;


// union definition used in parser decaf.y
union Node{
    int ival;
    bool bval;
    char cval;
    char *sval;
    ASTProgram* program;
    vector<ASTFieldDeclaration*>* field_declarations;
    vector<ASTMethodDeclaration*>* method_declarations;
    vector<ASTIdComma*>* id_comma;
    ASTFieldDeclaration* field_declaration;
    ASTMethodDeclaration* method_declaration;
    vector<ASTArgument*>* arguments;
    ASTArgument* argument;
    ASTBlock* block;
    vector<ASTVarDeclaration*>* var_declarations;
    ASTVarDeclaration* var_declaration;
    vector<ASTStatement*>* statements;
    ASTStatement* statement;
    vector<ASTIdCommas*>* id_commas;
    vector<ASTExpression*>* expr_comma;
    ASTLocation* location;
    ASTExpression* expr;
    ASTLiteral* literal;
    ASTAssignOperator* assign_op;
    ASTMethodCall* method_call;
    vector<ASTCalloutArgument*>* callout_arg_comma;
};

typedef union Node YYSTYPE;

// arithematic operations
enum class arithematic_op {
    plus_op,  
    minus_op, 
    multiply_op,
    divide_op,  
    modulo_op,
    and_and,
    or_or,
    less_than,
    more_than,
    less_than_equal,
    more_than_equal,
    equal_equal,
    not_equal,
    unot
};

// assignment orperators
enum class assign_op {
    equal,
    plus_equal,
    minus_equal
};

// datatypes 
enum class datatype {
    int_type,
    bool_type,
    void_type
};

// Class Definitions for AST nodes. Each class corresponds to a grammar symbol.
// ASTProgram is top-level node in the Abstract Syntax Tree.

/*  Prototype of classes.(May vary)
 *  private :
 *        variables
 *  public :
 *        constructor()
 *        destructor()
 *        get<Variable name>() ( methods for accessing variables private to class )
 *        accept()  (optional)
 *
 */

class ASTProgram {
    vector<ASTFieldDeclaration*> *fieldDecls;
    vector<ASTMethodDeclaration*> *methodDecls;
    public:
        ASTProgram(vector<ASTFieldDeclaration*>* fieldDecls, vector<ASTMethodDeclaration*>* methodDecls) {
            this->fieldDecls = fieldDecls;
            this->methodDecls = methodDecls;
        }
        
        ~ASTProgram() {
        }
        
        vector<ASTFieldDeclaration*>* getFieldDecls() {
            return this->fieldDecls;
        }
        
        vector<ASTMethodDeclaration*>* getMethodDecls() {
            return this->methodDecls;
        }
};


class ASTFieldDeclaration {
    datatype type;
    vector < ASTIdComma *> * idList;
        public:
            ASTFieldDeclaration(datatype type, vector < ASTIdComma *> * idList){
                this -> type = type;
                this -> idList = idList;
            }
            ~ASTFieldDeclaration(){}
            datatype getType(){ return this -> type; }
            vector < ASTIdComma *> * getIdList(){ return this -> idList; }
            void  * accept(visitor * v) { 
                return v -> visit(this); 
            }
};

class ASTIdComma {
    public:
        ASTIdComma(){};
        ~ASTIdComma(){};
        virtual void * accept(visitor * v) = 0;      
};

class ASTVarIdentifier : public ASTIdComma{
    string id;
    public:
        ASTVarIdentifier(string id) { this -> id = id; };
        ~ASTVarIdentifier(){};

        string getId(){ return this -> id; }
        void  * accept(visitor * v) { 
            return v -> visit(this);
        }
};

class ASTArrayIdentifier : public ASTIdComma{
    string id;
    int size;
    public:
        ASTArrayIdentifier(string id, int size){
            this -> id = id;
            this -> size = size;
        }
        ~ASTArrayIdentifier(){};

        string getId(){ return this -> id; }
        int getSize(){ return this -> size; }
        void * accept(visitor * v) { 
            return v -> visit(this);
        }
};


class ASTMethodDeclaration {
    datatype type;
    string id;
    vector<ASTArgument*>* argument;
    ASTBlock* block;
    
    public:
        ASTMethodDeclaration(datatype type, string id, vector<ASTArgument*>* argument, ASTBlock* block) {
            this->type = type;
            this->id = id;
            this->argument = argument;
            this->block = block;
        }
        
        ~ASTMethodDeclaration() {
        }
        
        datatype getType() {
            return this->type;
        }
  
        string getId() {
            return this->id;
        }
  
        vector<ASTArgument*>* getArgument() {
            return this->argument;
        }
        
        ASTBlock* getBlock() {
            return this->block;
        }
        void accept(visitor* v) {
            //v->visit(this);
        }
};


class ASTArgument {
    datatype type;
    string id;
    public:
        ASTArgument(datatype type, string id) {
            this->type = type;
            this->id = id;
        }
        
        datatype getType() {
            return this->type;
        }
  
        string getId() {
            return this->id;
        }
  
        ~ASTArgument() {

        }
        void accept(visitor* v) {
            //v->visit(this);
        }
};


class ASTBlock {
    vector< ASTVarDeclaration*>* varDecls;
    vector< ASTStatement*>* statements;
    public:
        ASTBlock(vector<ASTVarDeclaration*>* varDecls, vector<ASTStatement*>* statements) {
            this->varDecls = varDecls;
            this->statements = statements;
        }
        ~ASTBlock() {

        }
        vector<ASTVarDeclaration*>* getVarDecls() {
            return this->varDecls;
        }
        vector<ASTStatement*>* getStatements() {
            return this->statements;
        }
        void accept(visitor* v) {
            //v->visit(this);
        }
};

class ASTVarDeclaration {
    datatype type;
    vector<ASTIdCommas*>* idCommas;
    public:
        ASTVarDeclaration(datatype type, vector<ASTIdCommas*>* idCommas) {
            this->type = type;
            this->idCommas = idCommas;
        }
        ~ASTVarDeclaration(){
          
        }
        datatype getType() {
            return this->type;
        }
        vector<ASTIdCommas*>* getIdCommas() {
            return this->idCommas;
        }
        void accept(visitor* v) {
            v->visit(this);
        }
};


class ASTStatement {
    public:
        ASTStatement() {
          
        }
        ~ASTStatement() {
          
        }

        virtual void* accept(visitor* v) = 0;
};

class ASTIdCommas {
      string id;
      public:
        ASTIdCommas(string id) {
            this->id = id;
        }
        ~ASTIdCommas() {
          
        }
        string getId() {
            return this->id;
        }
        void accept(visitor* v) {
            //v->visit(this);
        }
};

class ASTBreakStatement: public ASTStatement {
    public:
        ASTBreakStatement() {
        }
        ~ASTBreakStatement() {
        }
        
        void *accept(visitor* v) {
            return v->visit(this);
        }
};

class ASTExpression {
      public:
        ASTExpression() {
        }
        ~ASTExpression() {
        }
        
        virtual void * accept(visitor* v) = 0;
};


class ASTBinaryExpression : public ASTExpression {
    ASTExpression* lhs, *rhs;
    arithematic_op oper;
    
    public:
        ASTBinaryExpression(ASTExpression* lhs, arithematic_op oper, ASTExpression* rhs) {
            this->lhs = lhs;
            this->oper = oper;
            this->rhs = rhs;
        }
        
        ~ASTBinaryExpression() {
        }
        
        ASTExpression* getLhs() {
            return this->lhs;
        }
        
        ASTExpression* getRhs() {
            return this->rhs;
        }
        
        arithematic_op getOper() {
            return this->oper;
        }
        
        void * accept(visitor* v) {
            return v->visit(this);
        }
        
};

class ASTUnaryExpression : public ASTExpression {
    ASTExpression* expr;
    arithematic_op oper;
    
    public:
        ASTUnaryExpression(arithematic_op oper, ASTExpression* expr) {
            this->expr = expr;
            this->oper = oper;
        }
        
        ~ASTUnaryExpression() {
        }
        
        ASTExpression* getExpr() {
            return this->expr;
        }
        
        arithematic_op getOper() {
            return this->oper;
        }
        
        void * accept(visitor* v) {
            return v->visit(this);
        }
};


class ASTReturnStatement: public ASTStatement {
    ASTExpression* expr;
    public:
        ASTReturnStatement(ASTExpression* expr) {
            this->expr = expr;
        }
        ~ASTReturnStatement() {
        }
        ASTExpression* getExpr() {
            return this->expr;
        }
        void * accept(visitor* v) {
            return v->visit(this);
        }
};


class ASTContinueStatement: public ASTStatement {
    public:
        ASTContinueStatement() {
        }
        ~ASTContinueStatement() {
        }
  
        void * accept(visitor* v) {
            return v->visit(this);
        }
};

class ASTIfStatement: public ASTStatement {
    ASTExpression* expr;
    ASTBlock* block;
    public:
        ASTIfStatement(ASTExpression* expr, ASTBlock* block) {
            this->expr = expr;
            this->block = block;
        }
  
        ~ASTIfStatement() {
        }
  
        ASTExpression* getExpr() {
            return this->expr;
        }
        
        ASTBlock* getBlock() {
            return this->block;
        }
        void * accept(visitor* v) {
            return v->visit(this);
        }
};

class ASTAssignmentStatement : public ASTStatement {
    ASTLocation* location;
    ASTAssignOperator* oper;
    ASTExpression* expr;
    public:
        ASTAssignmentStatement(ASTLocation* location, ASTAssignOperator* oper, ASTExpression* expr) {
            this->location = location;
            this->oper = oper;
            this->expr = expr;
        }
        ~ASTAssignmentStatement() {

        }
        ASTLocation* getLocation() {
            return this->location;
        }
        ASTAssignOperator* getOper() {
            return this->oper;
        }
        ASTExpression* getExpr() {
            return this->expr;
        }
        void * accept(visitor* v) {
            return v->visit(this);
        }

};


class ASTForStatement: public ASTStatement {
    string id;
    ASTExpression* expr1, *expr2;
    ASTBlock* block;
    public:
        ASTForStatement(string id, ASTExpression* expr1, ASTExpression* expr2, ASTBlock* block) {
            this->id = id;
            this->expr1 = expr1;
            this->expr2 = expr2;
            this->block = block;
        }

        ~ASTForStatement() {

        }

        string getId() {
            return this->id;
        }

        ASTExpression* getExpr1() {
            return this->expr1;
        }

        ASTExpression* getExpr2() {
            return this->expr2;
        }

        ASTBlock* getBlock() {
            return this->block;
        }

        void * accept(visitor* v) {
            return v->visit(this);
        }
};

class ASTIfElseStatement: public ASTStatement {
    ASTExpression* expr;
    ASTBlock* ifBlock;
    ASTBlock* elseBlock;
    public:
        ASTIfElseStatement(ASTExpression* expr, ASTBlock* ifBlock, ASTBlock* elseBlock) {
            this->expr = expr;
            this->ifBlock = ifBlock;
            this->elseBlock = elseBlock;
        }
  
        ~ASTIfElseStatement() {
        }
        
        ASTExpression* getExpr() {
            return this->expr;
        }
        ASTBlock* getIfBlock() {
            return this->ifBlock;
        }
        ASTBlock* getElseBlock() {
            return this->elseBlock;
        }
       void* accept(visitor* v) {
            return v->visit(this);
        }
};


class ASTBlockStatement: public ASTStatement {
    ASTBlock* block;
    public:
        ASTBlockStatement(ASTBlock* block) {
            this->block = block;
        }
        ~ASTBlockStatement() {
        }
        ASTBlock* getBlock() {
            return this->block;
        }
        void * accept(visitor* v) {
            return v->visit(this);
        }
        
};


class ASTLocation : public ASTExpression {
    public:
        ASTLocation() {
        }
        ~ASTLocation() {
        }
        virtual void * accept(visitor* v) = 0;
};


class ASTVarLocation : public ASTLocation {
    string id;
    public:
        ASTVarLocation(string id) {
            this->id = id;
        }
        ~ASTVarLocation(){
        }
        string getId() {
            return this->id;
        }
  
        void * accept(visitor* v) {
            return v->visit(this);
        }
};


class ASTArrayLocation : public ASTLocation {
    string id;
    ASTExpression* expr;
    public:
        ASTArrayLocation(string id, ASTExpression* expr) {
            this->id = id;
            this->expr = expr;
        }
        ~ASTArrayLocation() {
        }
        string getId() {
            return this->id;
        }
        ASTExpression* getExpr(){
          return this -> expr;
        }
        void * accept(visitor* v) {
            return v->visit(this);
        }
};

class ASTLiteral : public ASTExpression {
    public:
        ASTLiteral() {
        }
        ~ASTLiteral() {
        }
        virtual void* accept(visitor* v) = 0;
};

class ASTIntLiteral : public ASTLiteral {
    int value;
    public:
        ASTIntLiteral(int value) {
            this->value = value;
        }
        ~ASTIntLiteral() {
        }
        int getValue() {
            return this->value;
        }
        void * accept(visitor* v) {
            return v->visit(this);
        }
};

class ASTCharLiteral : public ASTLiteral {
    char value;
    public:
        ASTCharLiteral(char value) {
            this->value = value;
        }
        ~ASTCharLiteral() {
        }
        char getValue() {
            return this->value;
        }
        void * accept(visitor* v) {
            return v->visit(this);
        }
};


class ASTBoolLiteral : public ASTLiteral {
    bool value;
    public:
        ASTBoolLiteral(bool value) {
            this->value = value;
        }
        ~ASTBoolLiteral() {
        }
        bool getValue() {
            return this->value;
        }
        void * accept(visitor* v) {
            return v->visit(this);
        }
};


class ASTAssignOperator {
    assign_op oper;
    public:
        ASTAssignOperator(assign_op oper) {
            this->oper = oper;
        }
        ~ASTAssignOperator() {

        }

        assign_op getOper() {
            return this->oper;
        }

        void * accept(visitor* v) {
            //return v->visit(this);
            return NULL;
        }

};


class ASTMethodCall : public ASTStatement, public ASTExpression {
    public:
        ASTMethodCall() {

        }
        ~ASTMethodCall() {

        }

        virtual void * accept(visitor* v) = 0;
};

class ASTNameMethodCall : public ASTMethodCall {
    string id;
    vector<ASTExpression*>* exprComma;
    public:
        ASTNameMethodCall(string id, vector<ASTExpression*>* exprComma) {
            this->id = id;
            this->exprComma = exprComma;
        }
        ~ASTNameMethodCall() {

        }

        string getId() {
            return this->id;
        }

        vector<ASTExpression*>* getExprComma() {
            return this->exprComma;
        }

        void * accept(visitor* v) {
            return v->visit(this);
        }
};


class ASTCallout : public ASTMethodCall {
    string value;
    vector<ASTCalloutArgument*>* args;
    public:
        ASTCallout(string value, vector<ASTCalloutArgument*>* args) {
            //cout << "in cons:" << endl;
            //cout << value << endl;
            this->value = value;
            this->args = args;
        }
        ~ASTCallout() {

        }
        string getValue() {
            return this->value;
        }
        vector<ASTCalloutArgument*>* getArgs() {
            return this->args;
        }
        void * accept(visitor* v) {
            return v->visit(this);
        }

};

class ASTCalloutArgument {
    public:
        ASTCalloutArgument() {

        }
        ~ASTCalloutArgument() {

        }
        virtual void * accept(visitor* v) = 0;
};

class ASTCalloutArgumentExpr : public ASTCalloutArgument {
    ASTExpression* expr;
    public:
        ASTCalloutArgumentExpr(ASTExpression* expr) {
            this->expr = expr;
        }
        ~ASTCalloutArgumentExpr() {

        }

        ASTExpression* getExpr() {
            return this->expr;
        }
        void * accept(visitor* v ) {
            return v->visit(this);
        }
};

class ASTCalloutArgumentString : public ASTCalloutArgument {
    string value;
    public:
        ASTCalloutArgumentString(string value) {
            this->value = value;
        }
        ~ASTCalloutArgumentString() {

        }
        string getValue() {
            return this->value;
        }
        void * accept(visitor* v) {
            return v->visit(this);
        }

};

/*  CodeGenerator class for generating the LLVM IR
 *  Derived class extending abstract visitor class.
 */

class CodeGenVisitor : public visitor {
    private:
        llvm::Module * module;
        ASTProgram * start;
        llvm::Function * mainFunction;
        SymbolTable symbolTable;
    public:
        // Constructor 
        CodeGenVisitor(ASTProgram * start) {
            module = new llvm::Module("palash-jugnu", llvm::getGlobalContext());
            module->setTargetTriple("x86_64-pc-linux-gnu");
            this->start = start;
            llvm::FunctionType *ftype = llvm::FunctionType::get(llvm::Type::getVoidTy(llvm::getGlobalContext()), false);
            mainFunction = llvm::Function::Create(ftype, llvm::GlobalValue::ExternalLinkage, "main", module);
        }

        // Destructor
        ~CodeGenVisitor() {
        }

        // Helper method for returning datatype.
        llvm::Type * parseType(datatype type) {
            switch(type) {
                case datatype::int_type: 
                    return llvm::Type::getInt64Ty(llvm::getGlobalContext());
                case datatype::void_type: 
                    return llvm::Type::getVoidTy(llvm::getGlobalContext());
                case datatype::bool_type: 
                    return llvm::Type::getInt64Ty(llvm::getGlobalContext());                   
            }
            return NULL;
        }
        
        // Main execution starts from here.
        // Invoked from main()

        void codeGen() {
            llvm::BasicBlock *block = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", mainFunction, 0);
            
            symbolTable.pushBlock(block);
            
            this->visit(start);
            block = symbolTable.topBlock();
            symbolTable.popBlock();
            
            llvm::ReturnInst::Create(llvm::getGlobalContext(), block);
            
            llvm::verifyModule(*module, llvm::PrintMessageAction);
            llvm::PassManager PM;
            PM.add(llvm::createPrintModulePass(&llvm::outs()));
            PM.run(*module);
        }

        // Error Handler Function to report errors.
        llvm::Value * ErrorHandler(const std::string error) {
            std::cerr << error <<std::endl;
            exit(0);
        }

        void * visit(ASTProgram * node) {
            if (node->getFieldDecls()) {
                for(auto it: *(node->getFieldDecls())) {
                    this->visit(it);
                }               
            }

            llvm::Function * iterator = NULL;
            llvm::Function * userMain = NULL;

            if (node->getMethodDecls()) {
                for(auto it = (node->getMethodDecls())->begin() ; it != (node->getMethodDecls())->end(); it++) {
                    // check if already such function exists in llvm symbol table and that is not also not main.
                    if(module->getFunction((*it)->getId()) && (*it)->getId() != "main") {
                        string s = module->getFunction((*it)->getId())->getName();
                        return ErrorHandler("Multiple Declaration of function: " + s);
                    }

                    // if main already exists then report error.
                    if ((*it)->getId() == "main" && userMain) {
                        return ErrorHandler("Multiple Declaration of main");
                    }
                    
                    iterator = static_cast<llvm::Function *>(this->visit(*it));
                    
                    // if encountering main for the first time then set userMain to iterator.
                    if ((*it)->getId() == "main" && !userMain) {
                        userMain = iterator;
                    }
                    // if main has got arguments which is semantically incorrent then report error.
                    if ((*it)->getId() == "main" && (*it)->getArgument()->size() != 0) {
                        string s = to_string((*it)->getArgument()->size());
                        return ErrorHandler("Main cannot have any arguments. Provided " + s);
                    }
                }               
            }
            // if no main is found then throw error.
            if (!userMain)
                return ErrorHandler("No main Found");
            else {
                // callinst represents a function call providing abstract to target
                // machine calling convention.
                // parameters in order from left to right:
                // llvm::Function, arguments, llvm::BasicBlock
                // THE EXECUTION BEGINS AT MAIN
                llvm::CallInst::Create(userMain, "", symbolTable.topBlock());
            }
            return NULL;
        }


        void * visit(ASTFieldDeclaration * node) {
            if(node->getIdList()) {
                for(auto it : *(node->getIdList()))
                     it->accept(this);
            }
            return NULL;
        }

        void * visit(ASTVarIdentifier* node) {
            llvm::GlobalVariable * globalInteger = new llvm::GlobalVariable(*module, llvm::Type::getInt64Ty(llvm::getGlobalContext()), false, llvm::GlobalValue::CommonLinkage, NULL, node->getId());
            globalInteger->setInitializer(llvm::ConstantInt::get(llvm::getGlobalContext(), llvm::APInt(64, llvm::StringRef("0"), 10)));
            symbolTable.declareLocalVariables(node->getId(), globalInteger);
            return globalInteger;
        }

        void * visit(ASTArrayIdentifier* node) {
            
            // check for invalid size parameter.
            if (node->getSize() <= 0) {
                string s = node->getId();
                return ErrorHandler("Invalid Size for Array: " + s + "\nSize should be greater than 0");
            }
            // since arrays cannot be declared inside function block therefore
            // it is implicit that they will be globally accessible. So here we
            // are doing the same thing as we did for global var identifiers aboves.
            llvm::GlobalVariable* variable = new llvm::GlobalVariable(*module, llvm::ArrayType::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), node->getSize()), false, llvm::GlobalValue::CommonLinkage, NULL, node->getId());
            variable->setInitializer(llvm::ConstantAggregateZero::get(llvm::ArrayType::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), node->getSize())));
            symbolTable.declareLocalVariables(node->getId(), variable);
            return variable;
        }

        void * visit(ASTMethodDeclaration* node) {
            std::vector<llvm::Type*> argTypes;
            // if arguments are present then for each of them
            // we push the types to a vector of type llvm::Type*
            if (node->getArgument()) {
                for (auto it : *(node->getArgument())) {
                    argTypes.push_back(parseType((it)->getType()));
                }
            }
            // creating a function type of the return type that can be boolean/integer/void.
            // arguments - which type it is of, vector of argument types(created above), boolean for representing isVariableArgs.
            llvm::FunctionType *ftype = llvm::FunctionType::get(parseType(node->getType()), llvm::makeArrayRef(argTypes), false);

            // now creating corresponding llvm function for it.
            // arguments - functionType, LinkageType(internal only here), string, top-level llvm module.
            llvm::Function *function = llvm::Function::Create(ftype, llvm::GlobalValue::InternalLinkage, node->getId(), module);

            // since function is followed by corresponding block so we will create a new basic block for it.
            llvm::BasicBlock *block = llvm::BasicBlock::Create(llvm::getGlobalContext(), "entry", function, 0); 
            symbolTable.pushBlock(block);
            if (node->getArgument()) {
                auto it2 = function->arg_begin();
                for (auto it = (node->getArgument())->begin(); it != (node->getArgument())->end(); it++, it2++) {
                    llvm::Value * arg = it2;
                    arg->setName((*it)->getId());
                    
                    llvm::AllocaInst * allocaInst  = new llvm::AllocaInst(llvm::Type::getInt64Ty(llvm::getGlobalContext()), (*it)->getId(), symbolTable.topBlock());
                    new llvm::StoreInst(arg, allocaInst, false, symbolTable.topBlock());
                    symbolTable.declareLocalVariables((*it)->getId(), allocaInst);
                }
            }
            

            this->visit(node->getBlock());
            if(!symbolTable.topBlock()->getTerminator()) {
                if(node->getType() == datatype::void_type)
                    llvm::ReturnInst::Create(llvm::getGlobalContext(), symbolTable.topBlock());
                else 
                    llvm::ReturnInst::Create(llvm::getGlobalContext(), llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), 0, true), symbolTable.topBlock());
            }

            symbolTable.popBlock();
            return function;
        }

        void * visit(ASTBlock* node) {
            // cout << "Block mila hai" << endl;

            if(node->getVarDecls()){
                for(auto it : *(node->getVarDecls())) {
                    this->visit(it);
                }
            }
            if(node->getStatements()){
                for(auto it : *(node->getStatements())) {
                    this->visit(it);
                    // check if the statement was return, break or continue.
                    // if so stop evaluating the statements to follow and break the execution of
                    // the function at this point.                    
                    ASTReturnStatement * returnStatement = dynamic_cast<ASTReturnStatement *>(it);
                    ASTBreakStatement * breakStatement = dynamic_cast<ASTBreakStatement *>(it);
                    ASTContinueStatement * continueStatement = dynamic_cast<ASTContinueStatement *>(it);
                    if (returnStatement || breakStatement || continueStatement) 
                        break;
                }
            }
            return NULL;
        }

        void * visit(ASTVarDeclaration* node) {
            if(node->getIdCommas()) {
                //cout << "size" << node->getIdCommas()->size() << endl;
                for(auto it : *(node->getIdCommas()))
                    this->visit(it);
            }
            return NULL;
        }

        void * visit(ASTIdCommas* node) {
            // cout << node->getId() << endl;
            llvm::AllocaInst * allocaInst = new llvm::AllocaInst(llvm::Type::getInt64Ty(llvm::getGlobalContext()), node->getId(), symbolTable.topBlock());
            new llvm::StoreInst(llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), 0, true), allocaInst, false, symbolTable.topBlock());
            symbolTable.declareLocalVariables(node->getId(), allocaInst);
            return allocaInst;
        }

        void * visit(ASTStatement* node) {
            llvm::BasicBlock * block = symbolTable.topBlock();
            // get the first non-terminating instruction of the basic block
            // and then unlink it from the parent.
            if (block->getTerminator()) {
                // terminate any and all instructions which end the current block when there are still instructions to do
                llvm::Instruction * terminator = block->getTerminator();
                terminator->eraseFromParent();
            }

            return node->accept(this);

        }

        void * visit(ASTReturnStatement* node) {
            // cout << "returnStatement" << endl;
            llvm::Function * function = symbolTable.topBlock()->getParent();
            string s = function->getName();
            llvm::Type * type = function->getReturnType();
            if (type->isVoidTy()) {
                if(node->getExpr()) {
                    return ErrorHandler("Unknown Return for Void Type Function: " + s);
                } else {
                    return llvm::ReturnInst::Create(llvm::getGlobalContext(),symbolTable.topBlock());
                }
            } else {
                if (node->getExpr()) {
                    llvm::Value * expression = static_cast<llvm::Value *>(this->visit(node->getExpr()));
                    return llvm::ReturnInst::Create(llvm::getGlobalContext(), expression, symbolTable.topBlock());
                } else {
                    return ErrorHandler("Void Type Return for Non Void Function: " + s);
                }
            }
            return NULL;
        }

        void * visit(ASTExpression* node) {
            // cout << "ASTExpression encountered" << endl;
            return node->accept(this);
        }

        void * visit(ASTIntLiteral * node) {
            // cout << "int literal encountered" << endl;
            return llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), node->getValue(), true);
        }

        void * visit(ASTCharLiteral * node) {
            return llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), node->getValue(), true);
        }

        void * visit(ASTBinaryExpression * node) {
            if(node->getOper() == arithematic_op::plus_op)
                return llvm::BinaryOperator::Create(llvm::Instruction::Add, static_cast<llvm::Value*>(this->visit(node->getLhs())), static_cast<llvm::Value*>(this->visit(node->getRhs())), "tmp", symbolTable.topBlock());
            else if(node->getOper() == arithematic_op::minus_op)
                return llvm::BinaryOperator::Create(llvm::Instruction::Sub, static_cast<llvm::Value*>(this->visit(node->getLhs())), static_cast<llvm::Value*>(this->visit(node->getRhs())), "tmp", symbolTable.topBlock());
            else if(node->getOper() == arithematic_op::multiply_op)
                return llvm::BinaryOperator::Create(llvm::Instruction::Mul, static_cast<llvm::Value*>(this->visit(node->getLhs())), static_cast<llvm::Value*>(this->visit(node->getRhs())), "tmp", symbolTable.topBlock());
            else if(node->getOper() == arithematic_op::divide_op)
                return llvm::BinaryOperator::Create(llvm::Instruction::SDiv, static_cast<llvm::Value*>(this->visit(node->getLhs())), static_cast<llvm::Value*>(this->visit(node->getRhs())), "tmp", symbolTable.topBlock());
            else if(node->getOper() == arithematic_op::modulo_op)
                return llvm::BinaryOperator::Create(llvm::Instruction::SRem, static_cast<llvm::Value*>(this->visit(node->getLhs())), static_cast<llvm::Value*>(this->visit(node->getRhs())), "tmp", symbolTable.topBlock());
            else if(node->getOper() == arithematic_op::less_than)
                return new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SLT, static_cast<llvm::Value*>(this->visit(node->getLhs())), static_cast<llvm::Value*>(this->visit(node->getRhs())),"tmp", symbolTable.topBlock()), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", symbolTable.topBlock());
            else if(node->getOper() == arithematic_op::more_than)
                return new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SGT, static_cast<llvm::Value*>(this->visit(node->getLhs())), static_cast<llvm::Value*>(this->visit(node->getRhs())),"tmp", symbolTable.topBlock()), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", symbolTable.topBlock());
            else if(node->getOper() == arithematic_op::less_than_equal)
                return new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SLE, static_cast<llvm::Value*>(this->visit(node->getLhs())), static_cast<llvm::Value*>(this->visit(node->getRhs())),"tmp", symbolTable.topBlock()), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", symbolTable.topBlock());
            else if(node->getOper() == arithematic_op::more_than_equal)
                return new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SGE, static_cast<llvm::Value*>(this->visit(node->getLhs())), static_cast<llvm::Value*>(this->visit(node->getRhs())),"tmp", symbolTable.topBlock()), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", symbolTable.topBlock());
            else if(node->getOper() == arithematic_op::not_equal)
                return new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_NE, static_cast<llvm::Value*>(this->visit(node->getLhs())), static_cast<llvm::Value*>(this->visit(node->getRhs())),"tmp", symbolTable.topBlock()), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", symbolTable.topBlock());
            else if(node->getOper() == arithematic_op::equal_equal)
                return new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_EQ, static_cast<llvm::Value*>(this->visit(node->getLhs())), static_cast<llvm::Value*>(this->visit(node->getRhs())),"tmp", symbolTable.topBlock()), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", symbolTable.topBlock());
            else if(node->getOper() == arithematic_op::and_and) {
                llvm::Value * left_side = static_cast<llvm::Value*>(new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_NE, static_cast<llvm::Value*>(this->visit(node->getLhs())), llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), 0, true),"tmp", symbolTable.topBlock()), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", symbolTable.topBlock()));
                llvm::Value * right_side = static_cast<llvm::Value*>(new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_NE, static_cast<llvm::Value*>(this->visit(node->getRhs())), llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), 0, true),"tmp", symbolTable.topBlock()), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", symbolTable.topBlock()));
                return llvm::BinaryOperator::Create(llvm::Instruction::And, left_side, right_side, "tmp", symbolTable.topBlock());
            }
            
            else if(node->getOper() == arithematic_op::or_or) {
                //llvm::BasicBlock * entryBlock = symbolTable.topBlock();
/*
                //llvm::BasicBlock * tempBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "lhs", entryBlock->getParent());
                llvm::Instruction * left = static_cast<llvm::Instruction*>(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_NE, static_cast<llvm::Value*>(this->visit(node->getLhs())), llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), 0, true),"tmp", tempBlock));
                llvm::Value * left_side = static_cast<llvm::Value*>(new llvm::ZExtInst(left, llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", tempBlock));
                //llvm::BranchInst::
                
                llvm::BasicBlock * headerBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "rhs", entryBlock->getParent());
                llvm::Instruction * right = static_cast<llvm::Instruction*>(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_NE, static_cast<llvm::Value*>(this->visit(node->getRhs())), llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), 0, true),"tmp", headerBlock));
                llvm::Value * right_side = static_cast<llvm::Value*>(new llvm::ZExtInst(right, llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", headerBlock));
                

                llvm::BinaryOperator::Create(llvm::Instruction::Or, left_side, right_side, "tmp", tempBlock);
                if(!headerBlock->getTerminator()) {
                    llvm::BranchInst::Create(entryBlock, headerBlock);
                    llvm::Value * returnvalue = static_cast<llvm::Value*>(new llvm::ZExtInst(left_out, llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", remainingBlock));
                    return returnValue;
                }
                
                if(!remainingBlock->getTerminator()) {
                    llvm::BranchInst::Create(entryBlock, remainingBlock);
                    cout <<"hola" << endl;
                }
                llvm::BranchInst::Create(remainingBlock, headerBlock, left_side, entryBlock);
                
                
                //llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_NE, left_side, llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), 0, true));
                
*/
                llvm::Value * left_side = static_cast<llvm::Value*>(new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_NE, static_cast<llvm::Value*>(this->visit(node->getLhs())), llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), 0, true),"tmp", symbolTable.topBlock()), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", symbolTable.topBlock()));
                llvm::Value * right_side = static_cast<llvm::Value*>(new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_NE, static_cast<llvm::Value*>(this->visit(node->getRhs())), llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), 0, true),"tmp", symbolTable.topBlock()), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", symbolTable.topBlock()));
                return llvm::BinaryOperator::Create(llvm::Instruction::Or, left_side, right_side, "tmp", symbolTable.topBlock());              
            }
            return ErrorHandler("No Known BinaryOperator!!");
        }

        void * visit(ASTBlockStatement* node) {
            // cout << "Block Statement mila hai" << endl;
            symbolTable.pushBlock(NULL);
            this->visit(node->getBlock());
            symbolTable.popBlock();
            return NULL;
        }

        void * visit(ASTContinueStatement * node) {
            llvm::BasicBlock * block = symbolTable.getCS();
            
            if (!block) {
                return ErrorHandler("Incorrect continue usage!!");
            }

            llvm::BasicBlock * curBlock = symbolTable.topBlock();
            
            return llvm::BranchInst::Create(block, curBlock);
        }

        void * visit(ASTBoolLiteral * node) {
            return llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), node->getValue(), true);
        }

        void * visit(ASTUnaryExpression * node) {
            if(node->getOper() == arithematic_op::minus_op)
                return llvm::BinaryOperator::Create(llvm::Instruction::Sub, llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), 0, true), static_cast<llvm::Value*>(this->visit(node->getExpr())), "tmp", symbolTable.topBlock());
            else if(node->getOper() == arithematic_op::unot)
                return new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_EQ, llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), 0, true), static_cast<llvm::Value*>(this->visit(node->getExpr())),"tmp", symbolTable.topBlock()), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", symbolTable.topBlock());
            return ErrorHandler("No Known UnaryOperator!!");
        }

        void * visit(ASTNameMethodCall * node) {

            if (node->getId() == "main") {
                return ErrorHandler("Sorry you cannot call main()");
            }
            std::vector<llvm::Value *> args;
            llvm::Function * function = module->getFunction(node->getId());
            if (!function) {
                return ErrorHandler("No Definition of the Function with name " + node->getId() + " found");
            }
            if (!function->isVarArg() && (node->getExprComma()) && (function->getArgumentList().size() != node->getExprComma()->size())) {
                string expected = to_string(function->getArgumentList().size());
                string given = to_string(node->getExprComma()->size());
                return ErrorHandler("Mismatch in number of arguments. Given " + given + ", Expecting " + expected);
            }
            if (node->getExprComma()) {
                for (auto it = (node->getExprComma())->begin(); it != (node->getExprComma())->end(); it++) {
                    args.push_back(static_cast<llvm::Value *>(this->visit(*it)));
                }
            }
            if (function->getReturnType()->isVoidTy()) {
                return llvm::CallInst::Create(function, llvm::makeArrayRef(args), "", symbolTable.topBlock());
            }
            return llvm::CallInst::Create(function, llvm::makeArrayRef(args), node->getId(), symbolTable.topBlock());
        }

        void * visit(ASTVarLocation* node) {
            
            if (!symbolTable.lookupGlobalVariables(node->getId())) {
                return ErrorHandler("Variable " + node->getId() + " not declared");
            }
            llvm::Value * val = symbolTable.returnLocalVariables(node->getId());
            if (val)
                return new llvm::LoadInst(val, "tmp", symbolTable.topBlock());
            return ErrorHandler("Variable " + node->getId() + " UnInitilized");
            
        }

        void * visit(ASTArrayLocation * node) {
            // Since arrays cannot be declared inside function thus
            // we only need to check it in global variables that if it exist
            // or not.
            if (!symbolTable.lookupGlobalVariables(node->getId())) {
                return ErrorHandler("No Such global Array Found");
            }
            std::vector <llvm::Value *> index;
            index.push_back(llvm::ConstantInt::get(llvm::getGlobalContext(), llvm::APInt(64, llvm::StringRef("0"), 10)));
            index.push_back(static_cast<llvm::Value *>(this->visit(node->getExpr())));
            llvm::Value * val = symbolTable.returnLocalVariables(node->getId());
            llvm::Value * offset = llvm::GetElementPtrInst::CreateInBounds(val, index, "tmp", symbolTable.topBlock());
            if (val) {
                llvm::LoadInst * load = new llvm::LoadInst(offset, "tmp", symbolTable.topBlock());
                return load;
            }
            return ErrorHandler("Variable UnInitilized");
        }

        void * visit(ASTBreakStatement * node) {
            llvm::BasicBlock * block = symbolTable.getBS();
            if (!block) {
                return ErrorHandler("Incorrect break usage");
            }
            llvm::BasicBlock * curBlock = symbolTable.topBlock();
            auto localVariables = symbolTable.getLocalVariable();
            symbolTable.popBlock();
            symbolTable.pushBlock(block);
            symbolTable.setLocalVariables(localVariables);
            return llvm::BranchInst::Create(block, curBlock);
        }

        void * visit(ASTCallout * node) {
            //cout <<  node->getValue() << endl;
            llvm::Function * function = module->getFunction(node->getValue());
            if (!function) {
                //cout << "print mila " << endl;
                llvm::FunctionType *ftype = llvm::FunctionType::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), true);
                function = llvm::Function::Create(ftype, llvm::GlobalValue::ExternalLinkage, node->getValue(), module);
            } 
            std::vector<llvm::Value *> args;
            //cout << node->getArgs()->size() << endl;
            if (node->getArgs()) {
                for (auto it : *(node->getArgs())) {
                    args.push_back(static_cast<llvm::Value *>(this->visit(it)));
                }
            }
            llvm::CallInst *call = llvm::CallInst::Create(function, llvm::makeArrayRef(args), node->getValue(), symbolTable.topBlock());
            return call;
        }
        
        void * visit(ASTCalloutArgument * node) {
            return node->accept(this);
        }

        void * visit(ASTCalloutArgumentString * node) {
            std::string argument = node->getValue();
            //cout << argument << endl;
            llvm::GlobalVariable* variable = new llvm::GlobalVariable(*module, llvm::ArrayType::get(llvm::IntegerType::get(llvm::getGlobalContext(), 8), argument.size() + 1), true, llvm::GlobalValue::InternalLinkage, NULL, "string");
            variable->setInitializer(llvm::ConstantDataArray::getString(llvm::getGlobalContext(), argument, true));
            return variable;
        }

        void * visit(ASTCalloutArgumentExpr * node) {
            return this->visit(node->getExpr());
        }

        void * visit(ASTAssignmentStatement * node) {
            llvm::Value * location = NULL;
            llvm::Value * existingValue = NULL;
            ASTVarLocation * varLocation = dynamic_cast<ASTVarLocation *>(node->getLocation());
            ASTArrayLocation * arrayLocation = dynamic_cast<ASTArrayLocation *>(node->getLocation());
            if (arrayLocation) {
                if (!symbolTable.lookupGlobalVariables(arrayLocation->getId())) {
                    return ErrorHandler("Variable " + arrayLocation->getId() + " not Declared");
                }
                std::vector <llvm::Value *> index;
                index.push_back(llvm::ConstantInt::get(llvm::getGlobalContext(), llvm::APInt(64, llvm::StringRef("0"), 10)));
                index.push_back(static_cast<llvm::Value *>(this->visit(arrayLocation->getExpr())));
                llvm::Value * val = symbolTable.returnLocalVariables(arrayLocation->getId());
                location = llvm::GetElementPtrInst::CreateInBounds(val, index, "tmp", symbolTable.topBlock());
            }
            if (varLocation) {
                if (!symbolTable.lookupGlobalVariables(varLocation->getId())) {
                    return ErrorHandler("Variable " + varLocation->getId() + " not Declared");
                }
                location = symbolTable.returnLocalVariables(varLocation->getId());
            }
            llvm::Value * expr = static_cast<llvm::Value *>(this->visit(node->getExpr()));
            
            switch(node->getOper()->getOper()) {
                case assign_op::plus_equal:
                    existingValue = new llvm::LoadInst(location, "load", symbolTable.topBlock());
                    expr = llvm::BinaryOperator::Create(llvm::Instruction::Add, existingValue, expr, "tmp", symbolTable.topBlock());
                    break;
                case assign_op::minus_equal: 
                    existingValue = new llvm::LoadInst(location, "load", symbolTable.topBlock());
                    expr = llvm::BinaryOperator::Create(llvm::Instruction::Sub, existingValue, expr, "tmp", symbolTable.topBlock());
                    break;
                case assign_op::equal: 
                    break;
            }
            if (!expr->getType()->isIntegerTy(64)) {
                return ErrorHandler("Right Hand Side of Assignment Statement Invalid.");
            }
            if (!location->getType()->isPointerTy()) {
                return ErrorHandler("Left Hand Side of Assignment Statement Invalid.");
            }
            return new llvm::StoreInst(expr, location, false, symbolTable.topBlock());
        }

        void * visit(ASTIfStatement * node) {
            llvm::BasicBlock * entryBlock = symbolTable.topBlock();
            llvm::Value * condition = static_cast<llvm::Value *>(this->visit(node->getExpr()));
            llvm::ICmpInst * comparison = new llvm::ICmpInst(*entryBlock, llvm::ICmpInst::ICMP_NE, condition, llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), 0, true), "tmp");
            llvm::BasicBlock * ifBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "ifBlock", entryBlock->getParent());
            llvm::BasicBlock * mergeBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "mergeBlock", entryBlock->getParent());

            llvm::BasicBlock * returnedBlock = NULL;

            symbolTable.pushBlock(ifBlock);
            this->visit(node->getBlock());
            returnedBlock = symbolTable.topBlock();
            symbolTable.popBlock();
            if (!returnedBlock->getTerminator()) {
                llvm::BranchInst::Create(mergeBlock, returnedBlock);
            }
            
            llvm::BranchInst::Create(ifBlock, mergeBlock, comparison, entryBlock);
            auto localVariables = symbolTable.getLocalVariable();
            symbolTable.popBlock();
            symbolTable.pushBlock(mergeBlock);
            symbolTable.setLocalVariables(localVariables);
            return NULL;
        }

        void * visit(ASTIfElseStatement* node) {
            llvm::BasicBlock * entryBlock = symbolTable.topBlock();
            llvm::Value * condition = static_cast<llvm::Value *>(this->visit(node->getExpr()));
            llvm::ICmpInst * comparison = new llvm::ICmpInst(*entryBlock, llvm::ICmpInst::ICMP_NE, condition, llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), 0, true), "tmp");
            llvm::BasicBlock * ifBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "ifBlock", entryBlock->getParent());
            llvm::BasicBlock * mergeBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "mergeBlock", entryBlock->getParent());

            llvm::BasicBlock * returnedBlock = NULL;

            symbolTable.pushBlock(ifBlock);
            this->visit(node->getIfBlock());
            returnedBlock = symbolTable.topBlock();
            symbolTable.popBlock();
            if (!returnedBlock->getTerminator()) {
                llvm::BranchInst::Create(mergeBlock, returnedBlock);
            }
            llvm::BasicBlock * elseBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "elseBlock", entryBlock->getParent());

            symbolTable.pushBlock(elseBlock);
            this->visit(node->getElseBlock());
            returnedBlock = symbolTable.topBlock();
            symbolTable.popBlock();
            if (!returnedBlock->getTerminator()) {
                llvm::BranchInst::Create(mergeBlock, returnedBlock);
            }
            llvm::BranchInst::Create(ifBlock, elseBlock, comparison, entryBlock);
            auto localVariables = symbolTable.getLocalVariable();
            symbolTable.popBlock();
            symbolTable.pushBlock(mergeBlock);
            symbolTable.setLocalVariables(localVariables);
            return NULL;
        }

         void * visit(ASTForStatement * node) {
            if (!symbolTable.lookupGlobalVariables(node->getId())) {
                return ErrorHandler("Variable Not Declared");
            }
            llvm::BasicBlock * entryBlock = symbolTable.topBlock();
            llvm::BasicBlock * headerBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop_header", entryBlock->getParent(), 0);
            llvm::BasicBlock * bodyBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "loop_body", entryBlock->getParent(), 0);
            llvm::BasicBlock * afterLoopBlock = llvm::BasicBlock::Create(llvm::getGlobalContext(), "after_loop", entryBlock->getParent(), 0);

            symbolTable.pushBCS(afterLoopBlock, headerBlock);

            new llvm::StoreInst(static_cast<llvm::Value *>(this->visit(node->getExpr1())), symbolTable.returnLocalVariables(node->getId()), false, entryBlock);

            //llvm::Value * val = new llvm::LoadInst(symbolTable.returnLocalVariables(node->getId()), "load", headerBlock);
            // llvm::Value * condition = static_cast<llvm::Value *>(this->visit(node->getExpr2()));
            llvm::Value * condition = NULL;
            ASTBinaryExpression* x = dynamic_cast<ASTBinaryExpression*>(node->getExpr2());
            if(x) {
                if(x->getOper() == arithematic_op::less_than) {
                    ASTVarLocation * varLocation = dynamic_cast<ASTVarLocation*>(x->getLhs());
                    if(varLocation) {
                        if (!symbolTable.lookupGlobalVariables(varLocation->getId())) {
                            return ErrorHandler("Variable " + varLocation->getId() + " not declared");
                        }
                        llvm::Value * val = symbolTable.returnLocalVariables(varLocation->getId());
                        if (val) {
                            ASTIntLiteral * intLiteral  = dynamic_cast<ASTIntLiteral*>(x->getRhs());
                            ASTVarLocation * RHSvarLocation = dynamic_cast<ASTVarLocation*>(x->getRhs());
                            if(intLiteral)
                                condition = new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SLT, static_cast<llvm::Value*>(new llvm::LoadInst(val, "tmp", headerBlock)), static_cast<llvm::Value*>(llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), intLiteral->getValue(), true)),"tmp", headerBlock), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", headerBlock);
                            else if(RHSvarLocation) {
                                if (!symbolTable.lookupGlobalVariables(RHSvarLocation->getId())) {
                                    return ErrorHandler("Variable " + RHSvarLocation->getId() + " not declared");
                                }
                                llvm::Value * value = symbolTable.returnLocalVariables(RHSvarLocation->getId());
                                if (value) {
                                    condition = new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SLT, static_cast<llvm::Value*>(new llvm::LoadInst(val, "tmp", headerBlock)), static_cast<llvm::Value*>(new llvm::LoadInst(value, "tmp", headerBlock)),"tmp", headerBlock), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", headerBlock);
                                }
                                else
                                    return ErrorHandler("Variable " + RHSvarLocation->getId() + " UnInitilized");
                            }
                            else {
                                return ErrorHandler("RHS Expression of For Loop is expected to be of type Integer Literal");
                            }
                        }
                        else {
                            return ErrorHandler("Variable " + varLocation->getId() + " UnInitilized");
                        }
                    }
                    else {
                        return ErrorHandler("LHS Expression of For Loop is expected to be of type Integer Variable");
                    }

                }
                else if(x->getOper() == arithematic_op::more_than) {
                    ASTVarLocation * varLocation = dynamic_cast<ASTVarLocation*>(x->getLhs());

                    if(varLocation) {
                        if (!symbolTable.lookupGlobalVariables(varLocation->getId())) {
                            return ErrorHandler("Variable " + varLocation->getId() + " not declared");
                        }
                        llvm::Value * val = symbolTable.returnLocalVariables(varLocation->getId());
                        if (val) {
                            ASTIntLiteral * intLiteral  = dynamic_cast<ASTIntLiteral*>(x->getRhs());
                            ASTVarLocation * RHSvarLocation = dynamic_cast<ASTVarLocation*>(x->getRhs());
                            if(intLiteral)
                                condition = new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SGT, static_cast<llvm::Value*>(new llvm::LoadInst(val, "tmp", headerBlock)), static_cast<llvm::Value*>(llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), intLiteral->getValue(), true)),"tmp", headerBlock), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", headerBlock);
                            else if(RHSvarLocation) {
                                if (!symbolTable.lookupGlobalVariables(RHSvarLocation->getId())) {
                                    return ErrorHandler("Variable " + RHSvarLocation->getId() + " not declared");
                                }
                                llvm::Value * value = symbolTable.returnLocalVariables(RHSvarLocation->getId());
                                if (value) {
                                    condition = new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SGT, static_cast<llvm::Value*>(new llvm::LoadInst(val, "tmp", headerBlock)), static_cast<llvm::Value*>(new llvm::LoadInst(value, "tmp", headerBlock)),"tmp", headerBlock), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", headerBlock);
                                }
                                else
                                    return ErrorHandler("Variable " + RHSvarLocation->getId() + " UnInitilized");
                            }
                            else {
                                return ErrorHandler("RHS Expression of For Loop is expected to be of type Integer Literal");
                            }
                        }
                        else {
                            return ErrorHandler("Variable " + varLocation->getId() + " UnInitilized");
                        }
                    }
                    else {
                        return ErrorHandler("LHS Expression of For Loop is expected to be of type Integer Variable");
                    }
                }
                else if(x->getOper() == arithematic_op::less_than_equal) {
                    ASTVarLocation * varLocation = dynamic_cast<ASTVarLocation*>(x->getLhs());
                    if(varLocation) {
                        if (!symbolTable.lookupGlobalVariables(varLocation->getId())) {
                            return ErrorHandler("Variable " + varLocation->getId() + " not declared");
                        }
                        llvm::Value * val = symbolTable.returnLocalVariables(varLocation->getId());
                        if (val) {
                            ASTIntLiteral * intLiteral  = dynamic_cast<ASTIntLiteral*>(x->getRhs());
                            ASTVarLocation * RHSvarLocation = dynamic_cast<ASTVarLocation*>(x->getRhs());
                            if(intLiteral)
                                condition = new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SLE, static_cast<llvm::Value*>(new llvm::LoadInst(val, "tmp", headerBlock)), static_cast<llvm::Value*>(llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), intLiteral->getValue(), true)),"tmp", headerBlock), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", headerBlock);
                            else if(RHSvarLocation) {
                                if (!symbolTable.lookupGlobalVariables(RHSvarLocation->getId())) {
                                    return ErrorHandler("Variable " + RHSvarLocation->getId() + " not declared");
                                }
                                llvm::Value * value = symbolTable.returnLocalVariables(RHSvarLocation->getId());
                                if (value) {
                                    condition = new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SLE, static_cast<llvm::Value*>(new llvm::LoadInst(val, "tmp", headerBlock)), static_cast<llvm::Value*>(new llvm::LoadInst(value, "tmp", headerBlock)),"tmp", headerBlock), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", headerBlock);
                                }
                                else
                                    return ErrorHandler("Variable " + RHSvarLocation->getId() + " UnInitilized");
                            }
                            else {
                                return ErrorHandler("RHS Expression of For Loop is expected to be of type Integer Literal");
                            }
                        }
                        else {
                            return ErrorHandler("Variable " + varLocation->getId() + " UnInitilized");
                        }
                    }
                    else {
                        return ErrorHandler("LHS Expression of For Loop is expected to be of type Integer Variable");
                    }
                }
                else if(x->getOper() == arithematic_op::more_than_equal) {
                    ASTVarLocation * varLocation = dynamic_cast<ASTVarLocation*>(x->getLhs());
                    if(varLocation) {
                        if (!symbolTable.lookupGlobalVariables(varLocation->getId())) {
                            return ErrorHandler("Variable " + varLocation->getId() + " not declared");
                        }
                        llvm::Value * val = symbolTable.returnLocalVariables(varLocation->getId());
                        if (val) {
                            ASTIntLiteral * intLiteral  = dynamic_cast<ASTIntLiteral*>(x->getRhs());
                            ASTVarLocation * RHSvarLocation = dynamic_cast<ASTVarLocation*>(x->getRhs());
                            if(intLiteral)
                                condition = new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SLE, static_cast<llvm::Value*>(new llvm::LoadInst(val, "tmp", headerBlock)), static_cast<llvm::Value*>(llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), intLiteral->getValue(), true)),"tmp", headerBlock), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", headerBlock);
                            else if(RHSvarLocation) {
                                if (!symbolTable.lookupGlobalVariables(RHSvarLocation->getId())) {
                                    return ErrorHandler("Variable " + RHSvarLocation->getId() + " not declared");
                                }
                                llvm::Value * value = symbolTable.returnLocalVariables(RHSvarLocation->getId());
                                if (value) {
                                    condition = new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_SLE, static_cast<llvm::Value*>(new llvm::LoadInst(val, "tmp", headerBlock)), static_cast<llvm::Value*>(new llvm::LoadInst(value, "tmp", headerBlock)),"tmp", headerBlock), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", headerBlock);
                                }
                                else
                                    return ErrorHandler("Variable " + RHSvarLocation->getId() + " UnInitilized");
                            }
                            else {
                                return ErrorHandler("RHS Expression of For Loop is expected to be of type Integer Literal");
                            }
                        }
                        else {
                            return ErrorHandler("Variable " + varLocation->getId() + " UnInitilized");
                        }
                    }
                    else {
                        return ErrorHandler("LHS Expression of For Loop is expected to be of type Integer Variable");
                    }
                }
                else if(x->getOper() == arithematic_op::not_equal) {
                    ASTVarLocation * varLocation = dynamic_cast<ASTVarLocation*>(x->getLhs());
                    if(varLocation) {
                        if (!symbolTable.lookupGlobalVariables(varLocation->getId())) {
                            return ErrorHandler("Variable " + varLocation->getId() + " not declared");
                        }
                        llvm::Value * val = symbolTable.returnLocalVariables(varLocation->getId());
                        if (val) {
                            ASTIntLiteral * intLiteral  = dynamic_cast<ASTIntLiteral*>(x->getRhs());
                            ASTVarLocation * RHSvarLocation = dynamic_cast<ASTVarLocation*>(x->getRhs());
                            //ASTVarLocation * varLocation1 = dynamic_cast<ASTIntLiteral*>(x->getRhs());
                            //ASTNameMethodCall * nameMethodCall = dynamic_cast<ASTNameMethodCall*>(x->getRhs());
                            if(intLiteral)
                                condition = new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_NE, static_cast<llvm::Value*>(new llvm::LoadInst(val, "tmp", headerBlock)), static_cast<llvm::Value*>(llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), intLiteral->getValue(), true)),"tmp", headerBlock), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", headerBlock);
                            /*else if(nameMethodCall) {
                                //cout << "Name method_call me ghus raha hu" << endl;
                                if (nameMethodCall->getId() == "main") {
                                    return ErrorHandler("Sorry you cannot call main()");
                                }
                                std::vector<llvm::Value *> args;
                                llvm::Function * function = module->getFunction(nameMethodCall->getId());
                                if (!function) {
                                    return ErrorHandler("No Definition of the Function with name " + nameMethodCall->getId() + " found");
                                }
                                if (!function->isVarArg() && (nameMethodCall->getExprComma()) && (function->getArgumentList().size() != nameMethodCall->getExprComma()->size())) {
                                    string expected = to_string(function->getArgumentList().size());
                                    string given = to_string(nameMethodCall->getExprComma()->size());
                                    return ErrorHandler("Mismatch in number of arguments. Given " + given + ", Expecting " + expected);
                                }
                                if (nameMethodCall->getExprComma()) {
                                    for (auto it = (nameMethodCall->getExprComma())->begin(); it != (nameMethodCall->getExprComma())->end(); it++) {
                                        args.push_back(static_cast<llvm::Value *>(this->visit(*it)));
                                    }
                                }
                                if (function->getReturnType()->isVoidTy()) {
                                    return ErrorHandler("Method should be of return type INT");
                                }


                                llvm::Value * returnValue;
                                condition = new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_NE, static_cast<llvm::Value*>(new llvm::LoadInst(val, "tmp", headerBlock)), static_cast<llvm::Value*>(new llvm::LoadInst(returnValue, static_cast<llvm::Value*>(llvm::CallInst::Create(function, llvm::makeArrayRef(args), nameMethodCall->getId()), headerBlock) )),"tmp", headerBlock), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", headerBlock);
                                
                            }*/
                            else if(RHSvarLocation) {
                                if (!symbolTable.lookupGlobalVariables(RHSvarLocation->getId())) {
                                    return ErrorHandler("Variable " + RHSvarLocation->getId() + " not declared");
                                }
                                llvm::Value * value = symbolTable.returnLocalVariables(RHSvarLocation->getId());
                                if (value) {
                                    condition = new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_NE, static_cast<llvm::Value*>(new llvm::LoadInst(val, "tmp", headerBlock)), static_cast<llvm::Value*>(new llvm::LoadInst(value, "tmp", headerBlock)),"tmp", headerBlock), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", headerBlock);
                                }
                                else
                                    return ErrorHandler("Variable " + RHSvarLocation->getId() + " UnInitilized");
                            }
                            else {
                                return ErrorHandler("RHS Expression of For Loop is expected to be of type Integer Literal");
                            }
                        }
                        else {
                            return ErrorHandler("Variable " + varLocation->getId() + " UnInitilized");
                        }
                    }
                    else {
                        return ErrorHandler("LHS Expression of For Loop is expected to be of type Integer Variable");
                    }
                }
                else if(x->getOper() == arithematic_op::equal_equal) {
                    ASTVarLocation * varLocation = dynamic_cast<ASTVarLocation*>(x->getLhs());
                    if(varLocation) {
                        if (!symbolTable.lookupGlobalVariables(varLocation->getId())) {
                            return ErrorHandler("Variable " + varLocation->getId() + " not declared");
                        }
                        llvm::Value * val = symbolTable.returnLocalVariables(varLocation->getId());
                        if (val) {
                            ASTIntLiteral * intLiteral  = dynamic_cast<ASTIntLiteral*>(x->getRhs());
                            ASTVarLocation * RHSvarLocation = dynamic_cast<ASTVarLocation*>(x->getRhs());
                            if(intLiteral)
                                condition = new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_EQ, static_cast<llvm::Value*>(new llvm::LoadInst(val, "tmp", headerBlock)), static_cast<llvm::Value*>(llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), intLiteral->getValue(), true)),"tmp", headerBlock), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", headerBlock);
                            else if(RHSvarLocation) {
                                if (!symbolTable.lookupGlobalVariables(RHSvarLocation->getId())) {
                                    return ErrorHandler("Variable " + RHSvarLocation->getId() + " not declared");
                                }
                                llvm::Value * value = symbolTable.returnLocalVariables(RHSvarLocation->getId());
                                if (value) {
                                    condition = new llvm::ZExtInst(llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_EQ, static_cast<llvm::Value*>(new llvm::LoadInst(val, "tmp", headerBlock)), static_cast<llvm::Value*>(new llvm::LoadInst(value, "tmp", headerBlock)),"tmp", headerBlock), llvm::Type::getInt64Ty(llvm::getGlobalContext()), "zext", headerBlock);
                                }
                                else
                                    return ErrorHandler("Variable " + RHSvarLocation->getId() + " UnInitilized");
                            }
                            else {
                                return ErrorHandler("RHS Expression of For Loop is expected to be of type Integer Literal");
                            }
                        }
                        else {
                            return ErrorHandler("Variable " + varLocation->getId() + " UnInitilized");
                        }
                    }
                    else {
                        return ErrorHandler("LHS Expression of For Loop is expected to be of type Integer Variable");
                    }
                }
                else {
                    return ErrorHandler("For loop's Condition Format Incorrect.\nCondition can only have operators <, >, <=, >=, == and != .");
                }
            } else {
                return ErrorHandler("Incorrect For Loop Usage!");
            }
            //llvm::ICmpInst * comparison = new llvm::ICmpInst(*headerBlock, llvm::ICmpInst::ICMP_NE, val, static_cast<llvm::Value *>(this->visit(node->getExpr2())), "tmp");
            
            llvm::ICmpInst * comparison = new llvm::ICmpInst(*headerBlock, llvm::ICmpInst::ICMP_NE, condition, llvm::ConstantInt::get(llvm::Type::getInt64Ty(llvm::getGlobalContext()), 0, true), "tmp");
            llvm::BranchInst::Create(bodyBlock, afterLoopBlock, comparison, headerBlock);
            llvm::BranchInst::Create(headerBlock, entryBlock);

            symbolTable.pushBlock(bodyBlock);
            this->visit(node->getBlock());
            bodyBlock = symbolTable.topBlock();
            symbolTable.popBlock();
            if (!bodyBlock->getTerminator()) {
                llvm::BranchInst::Create(headerBlock, bodyBlock);
            }

            auto localVariables = symbolTable.getLocalVariable();
            symbolTable.popBlock();
            symbolTable.pushBlock(afterLoopBlock);
            symbolTable.setLocalVariables(localVariables);

            symbolTable.popBCS();
            return NULL;
        }
};