all:
	csi -s compile.scm > output.s
	gcc -m64 -o test output.s runner.c
	echo "Program output:"
	./test
