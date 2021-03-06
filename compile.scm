(define wordsize 8)

(include "environment.scm")
(include "syntax/derived/and.scm")
(include "syntax/derived/or.scm")
(include "syntax/if.scm")
(include "syntax/let.scm")
(include "primitives.scm")
(include "procedures.scm")
(include "features/booleans.scm")
(include "features/pairs.scm")
(include "features/vectors.scm")
(include "features/fixnums.scm")
(include "features/chars.scm")
(include "features/type_conversions.scm")
(include "features/system.scm")
(include "features/string.scm")

(define (tagged-list? expr tag)
  (and (pair? expr)
       (eq? (car expr) tag)))

(define (immediate? x)
  (or (fixnum? x)
      (boolean? x)
      (char? x)
      (null? x)))

(define (immediate-rep x)
  (cond
    ((fixnum? x) 
     (fxshl x fixnum_shift))
    ((boolean? x)
     (if x bool_true bool_false))
    ((null? x) empty_list)
    ((char? x)
     (+ (fxshl (char->integer x) 8)
        char_tag))
    (else (error "Invalid expression: " x))))

(define (variable? expr) (symbol? expr))

(define (emit-label label)
  (emit label ":"))

(define (emit-comment . args)
  (apply emit (cons "  # " args)))

(define (next-stack-index stack-index) (next-stack-index-n stack-index 1))
(define (prev-stack-index stack-index) (prev-stack-index-n stack-index 1))

(define (next-stack-index-n stack-index n) (+ stack-index (* n wordsize)))
(define (prev-stack-index-n stack-index n) (- stack-index (* n wordsize)))

(define (emit-variable-ref env var tail)
  (let ((location (lookup var env)))
    (if location
        (emit-stack-load location)
        (else (error "Reference to unbound variable: " var)))))

(define (emit-stack-save stack-index env expr)
  (if (immediate? expr)
    (emit "  mov [rsp - " stack-index "], QWORD PTR " (immediate-rep expr))
    (begin
      (emit-expr stack-index env expr)
      (emit "  mov [rsp - " stack-index "], rax"))))

(define (emit-stack-load stack-index)
  (emit "  mov rax, [rsp - " stack-index "]"))

(define (emit . output)
  (apply print output))

(define (emit-immediate expr)
  (emit "  mov rax, QWORD PTR " (immediate-rep expr)))

(define (emit-expr stack-index env expr tail)
  (cond
    ((immediate? expr)
     (emit-immediate expr))
    ((if? expr)
     (emit-if stack-index env expr tail))
    ((and? expr) (emit-expr stack-index env (and->if expr) tail))
    ((or? expr) (emit-expr stack-index env (or->if expr) tail))
    ((let? expr) (emit-let stack-index env expr tail))
    ((let*? expr) (emit-let* stack-index env expr tail))
    ((apply? expr) (emit-apply stack-index env expr tail))
    ((string? expr) (emit-string stack-index env expr))
    ((primitive? expr)
     (emit-comment (car expr))
     ((lookup-primitive (car expr)) stack-index env (cdr expr))
     (emit-comment "end " (car expr)))
    ((predicate? expr)
       (let ((raw-predicate (lookup-raw-predicate (car expr)))
             (true-label (unique-label "true"))
             (end-label (unique-label "end")))
         (emit-comment (car expr))
         (raw-predicate stack-index env (cdr expr))
         (emit "  je " true-label)
         (emit-immediate #f)
         (emit "  jmp " end-label)
         (emit true-label ":")
         (emit-immediate #t)
         (emit end-label ":")
         (emit-comment "end " (car expr))))
    ((variable? expr)
     (emit-variable-ref env expr tail))
    (else
      (error "Unknown expression: " expr))))

(define (emit-program expr)
  (emit "  .intel_syntax noprefix")
  (emit "  .text")
  (if (letrec? expr)
      (emit-letrec expr #f)
      (emit-scheme-entry expr empty-env #f)))

(define (emit-scheme-entry expr env tail)
  (emit-function-header "scheme_body")
  (emit-expr wordsize env expr tail)
  (emit "  ret")
  (emit "  .text")
  (emit-function-header "scheme_entry")
  ; 64bit calling convention:
  ; RDI, RSI, RDX, RCX (R10 in the Linux kernel interface), R8, and R9
  ; ctxt: RDI
  ; stack_base: RSI
  ; heap: RDX
  (emit-comment "store register contents in ctxt")
  (emit "  mov r15, rdi")
  (emit "  mov [r15 + 8], rbx")
  (emit "  mov [r15 + 32], rsi")
  (emit "  mov [r15 + 40], rdi")
  (emit "  mov [r15 + 48], rbp")
  (emit "  mov [r15 + 56], rsp")
  (emit-comment "load stack_base and heap addresses")
  (emit "  mov rsp, rsi")
  (emit "  mov rbp, rdx")
  (emit "  call scheme_body")
  (emit-comment "store register contents in ctxt")
  (emit "  mov rbx, [r15 + 8]")
  (emit "  mov rsi, [r15 + 32]")
  (emit "  mov rdi, [r15 + 40]")
  (emit "  mov rbp, [r15 + 48]")
  (emit "  mov rsp, [r15 + 56]")
  (emit "  ret"))

(define (emit-function-header name)
  (emit "  .globl " name)
  (emit "  .type " name ", @function")
  (emit name ":"))

; (emit-program '(fxzero? 1))
; (emit-program '(boolean? #\A))
; (emit-program '(if #t (if (fxzero? 0) #\a #\b) #\c))
; (emit-program '(char->fixnum #\A))
; (emit-program '(fixnum->char 65))
; (emit-program '(not #t))
; (emit-program '(fxlognot -11))
; (emit-program 10)
; (emit-program '(or #f #f))
; (emit-program '(if (fxzero? 0) #\y #\n))
; (emit-program '(fx+ (fx- 1 2) 3))
; (emit-program
;   '(fx+ 10
;         (if #t 20 30)))
; (emit-program '(char<? #\Z #\B))
; (emit-program '(fx- 5 10)) 
; (emit-program '(let ((a 1) (b 2)) (fx+ a b)))
; (emit-program
;   '(let* ((x 1))
;      (let* ((x (fx+ x 1))
;            (y (fx+ x 1)))
;        y)))
; (emit-program
;   '(letrec
;      ((sum (lambda (n acc)
;             (if (fxzero? n)
;                 acc
;                 (apply sum (fxsub1 n) (fx+ n acc))))))
;      (apply sum 6000 0)))
; (emit-program
;   '(letrec
;      ((fac (lambda (n)
;             (if (fxzero? n)
;                 1
;                 (fx* n (apply fac (fxsub1 n)))))))
;      (apply fac 20)))
; (emit-program
;   '(letrec
;      ((fac (lambda (n)
;              (let ((a (fxzero? n)))
;                (if a
;                   1
;                   (let ((b (fxsub1 n)))
;                     (let ((c (apply fac b)))
;                       (fx* n c))))))))
;      (apply fac 20)))
; (emit-program '(cons (cons 1 2) (cons 3 4)))
; (emit-program '(vector-ref (make-vector 5 #t) 4))
; (emit-program '(let*
;                  ((x (make-vector 5 0))
;                   (y (vector-set! x 0 0))
;                   (z (vector-set! x 1 1))
;                   (w (vector-set! x 2 2))
;                   (v (vector-set! x 3 3))
;                   (u (vector-set! x 4 4)))
;                   (vector-ref x 3)))
; (emit-program '(fx/ 10 5))
; (emit-program '(sys-write "hello world"))
; (emit-program '(string-length (fixnum->string 123)))
(emit-program '(sys-write (fixnum->string 1234)))
; (emit-program '(fixnum->string 0))
