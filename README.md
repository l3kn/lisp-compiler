# LISP to x86-64 compiler

Based on “Compilers: Backend to Frontend and Back to Front Again”
by Abdulaziz Ghuloum

## Design Decisions

* No inlining
* Internal calling convention: rdi, rsi, rdx, rcx, r8, r9
  * No functions with >6 arguments

## TODO

- [x] Refactor `emit-tail` functions
- [x] Refactor primitives
- [x] Find a way to make the code for `cons` shorter
