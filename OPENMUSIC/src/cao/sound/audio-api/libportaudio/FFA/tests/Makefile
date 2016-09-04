test: libtest.so
	sbcl --eval '(progn (load "test.lisp") (quit))'

libtest.so: test.o
	gcc -shared -Wl,-soname,libtest.so -o libtest.so test.o

test.o: test.c
	gcc -c -fPIC test.c -o test.o -std=c99

