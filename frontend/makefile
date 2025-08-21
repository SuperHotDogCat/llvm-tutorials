main:
	clang++ -g -O3 main.cpp -o main.out `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native`
clean:
	rm *.out 
	rm -rf *.dSYM
