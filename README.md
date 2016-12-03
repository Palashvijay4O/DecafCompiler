# DecafCompiler

The code has been tested/written on Ubuntu 14.04 LTS and LLVM 3.4.
In case if you want docker image of the system, just open up an
issue.

How to use the compiler:
Run the below commands (in order)
	1. make clean
	2. make
	3. ./decaf \<input_filepath\> > \<output_filepath\>
	4. clang \<output_filepath\> 
	5. ./a.out
