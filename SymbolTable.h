#include <list>
#include <stack>
#include <map>
#include <string>
#include <utility>
#include <llvm/Analysis/Verifier.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#define all(c) c.begin(),c.end()

using namespace std;
// For every block there will be a new symbol table for handling scoping of variables.

class SymbolTableNode
{
    public:
        llvm::BasicBlock * block;
        // mapping from string -> llvm value of local variables.
        map<string, llvm::Value *> localVariables;

        SymbolTableNode(llvm::BasicBlock * block) {
            this->block = block;
        }
        ~SymbolTableNode() {

        }
};

class SymbolTable {
	public:
		// The complete symbol table of the program will be the list of
		// symbol tables from all the blocks.
		list <SymbolTableNode> table;

		// first of pair will tell from where to start new symbol table
		// second of pair will tell which block's symbol table it is.
		stack < pair <llvm::BasicBlock*, llvm::BasicBlock*> > bcs;

		SymbolTable() {

		}
		~SymbolTable() { 

		}

		map<string, llvm::Value *> getLocalVariable() {
			return this->table.front().localVariables;
		}

		void setLocalVariables(map<string, llvm::Value*> variables) {
			this->table.front().localVariables.insert(all(variables));
		}

		bool lookupLocalVariable(string variable) {
			auto it = this->getLocalVariable();
			return it.find(variable) != it.end();
		}

		void declareLocalVariables(string name, llvm::Value * value) {
            if (!this->lookupLocalVariable(name)) {
                this->table.front().localVariables.insert(std::pair<std::string, llvm::Value *>(name, value));
            } else {
                cout <<"Variable "<< name <<" already declared";
                exit(0);
            }
        }

        bool lookupGlobalVariables(string name) {
            return this->returnLocalVariables(name) != NULL;
        }

        llvm::Value * returnLocalVariables(string name) {
            for(auto it: this->table) {
            	if(it.localVariables.find(name) != it.localVariables.end())
            		return it.localVariables.find(name)->second;
            }
            return NULL;
        }

        void pushBlock(llvm::BasicBlock * block) {
            this->table.push_front(SymbolTableNode(block));
        }

        void popBlock() {
            this->table.pop_front();
        }

        llvm::BasicBlock * topBlock() {
            for (auto it : this->table) {
                if (it.block) {
                    return it.block;
                }
            }
            return this->table.front().block;
        }

        llvm::BasicBlock * bottomBlock() {
            return this->table.back().block;
        }

        void pushBCS(llvm::BasicBlock * breakStatement, llvm::BasicBlock * returnStatement) {
            this->bcs.push({breakStatement, returnStatement});
        }

        void popBCS() {
            this->bcs.pop();
        }

        llvm::BasicBlock * getBS() {
            if (!this->bcs.empty()) {
                return this->bcs.top().first;
            }
            return NULL;
        }

        llvm::BasicBlock * getCS() {
            if (!this->bcs.empty()) {
                return this->bcs.top().second;
            }
            return NULL;
        }
};