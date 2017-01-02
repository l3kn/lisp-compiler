all:
	csi -s compile.scm > output.s
	gcc -o test output.s runner.c
	echo "Program output:"
	./test
