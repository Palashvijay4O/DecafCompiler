all:
	bison -d decaf.y
	flex decaf.l
	g++ decaf.tab.c -c -O3 -flto -Wall  -std=c++11 `llvm-config --cppflags --libs all --ldflags`
	g++ lex.yy.c -c -O3 -flto -Wall -std=c++11 `llvm-config --cppflags --libs all --ldflags`
	g++ main.cpp -c -O3 -flto -Wall -std=c++11 `llvm-config --cppflags --libs all --ldflags`
	g++ -o decaf decaf.tab.o lex.yy.o main.o -O3 -flto -Wall -std=c++11 `llvm-config --cppflags --libs all --ldflags`


clean:
	rm -f decaf.tab.* lex.yy.* main.o decaf a.out *.ll