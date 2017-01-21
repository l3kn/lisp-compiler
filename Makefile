all:
	csi -s compile.scm > output.s
	gcc -m64 -masm=intel -o output primitives.s output.s output.s runner.c
	echo "Program output:"
	./output

primitives:
	csi -s compile-primitives.scm > primitives.s

