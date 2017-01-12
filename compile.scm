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

(define (emit-comment . args)
  (apply emit (cons "  # " args)))

(define (next-stack-index stack-index) (next-stack-index-n stack-index 1))
(define (prev-stack-index stack-index) (prev-stack-index-n stack-index 1))

(define (next-stack-index-n stack-index n) (+ stack-index (* n wordsize)))
(define (prev-stack-index-n stack-index n) (- stack-index (* n wordsize)))

(define (emit-variable-ref env var)
  (let ((location (lookup var env)))
    (if location
        (emit-stack-load location)
        (else (error "Reference to unbound variable: " var)))))

(define (emit-tail-variable-ref env var)
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

(define (emit-expr stack-index env expr)
  (cond
    ((immediate? expr)
     (emit-immediate expr))
    ((if? expr)
     (emit-if stack-index env expr))
    ((and? expr) (emit-expr stack-index env (and->if expr)))
    ((or? expr) (emit-expr stack-index env (or->if expr)))
    ((let? expr) (emit-let stack-index env expr))
    ((let*? expr) (emit-let* stack-index env expr))
    ((apply? expr) (emit-apply stack-index env expr))
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
     (emit-variable-ref env expr))
    (else
      (error "Unknown expression: " expr))))

(define (emit-tail-expr stack-index env expr)
  (cond
    ((immediate? expr)
     (emit-immediate expr)
     (emit "  ret"))
    ((if? expr)
     (emit-tail-if stack-index env expr))
    ((and? expr) (emit-tail-expr stack-index env (and->if expr)))
    ((or? expr) (emit-tail-expr stack-index env (or->if expr)))
    ((let? expr) (emit-tail-let stack-index env expr))
    ((let*? expr) (emit-tail-let* stack-index env expr))
    ((apply? expr) (emit-tail-apply stack-index env expr))
    ((primitive? expr)
     (emit-comment (car expr))
     ((lookup-primitive (car expr)) stack-index env (cdr expr))
     (emit-comment "end " (car expr))
     (emit " ret"))
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
     (emit-tail-variable-ref env expr)
     (emit "  ret"))
    (else
      (error "Unknown expression: " expr))))

(define (emit-program expr)
  (emit "  .intel_syntax noprefix")
  (emit "  .text")
  (if (letrec? expr)
      (emit-letrec expr)
      (emit-scheme-entry expr empty-env)))

(define (emit-scheme-entry expr env)
  (emit-function-header "scheme_body")
  (emit-expr wordsize env expr)
  (emit "  ret")
  (emit "  .text")
  (emit-function-header "scheme_entry")
  ; 64bit calling convention:
  ; RDI, RSI, RDX, RCX (R10 in the Linux kernel interface), R8, and R9
  ; ctxt: RDI
  ; stack_base: RSI
  ; heap: RDX
  (emit-comment "store register contents in ctxt")
  (emit "  mov rcx, rdi")
  (emit "  mov [rcx + 8], rbx")
  (emit "  mov [rcx + 32], rsi")
  (emit "  mov [rcx + 40], rdi")
  (emit "  mov [rcx + 48], rbp")
  (emit "  mov [rcx + 56], rsp")
  (emit-comment "load stack_base and heap addresses")
  (emit "  mov rsp, rsi")
  (emit "  mov rbp, rdx")
  (emit "  call scheme_body")
  (emit-comment "store register contents in ctxt")
  (emit "  mov rbx, [rcx + 8]")
  (emit "  mov rsi, [rcx + 32]")
  (emit "  mov rdi, [rcx + 40]")
  (emit "  mov rbp, [rcx + 48]")
  (emit "  mov rsp, [rcx + 56]")
  (emit "  ret"))

(define (emit-tail-scheme-entry expr env)
  (emit-function-header "scheme_body")
  (emit-tail-expr wordsize env expr)
  (emit "  ret")
  (emit "  .text")
  (emit-function-header "scheme_entry")
  ; 64bit calling convention:
  ; RDI, RSI, RDX, RCX (R10 in the Linux kernel interface), R8, and R9
  ; ctxt: RDI
  ; stack_base: RSI
  ; heap: RDX
  (emit-comment "store register contents in ctxt")
  (emit "  mov rcx, rdi")
  (emit "  mov [rcx + 8], rbx")
  (emit "  mov [rcx + 32], rsi")
  (emit "  mov [rcx + 40], rdi")
  (emit "  mov [rcx + 48], rbp")
  (emit "  mov [rcx + 56], rsp")
  (emit-comment "load stack_base and heap addresses")
  (emit "  mov rsp, rsi")
  (emit "  mov rbp, rdx")
  (emit "  call scheme_body")
  (emit-comment "store register contents in ctxt")
  (emit "  mov rbx, [rcx + 8]")
  (emit "  mov rsi, [rcx + 32]")
  (emit "  mov rdi, [rcx + 40]")
  (emit "  mov rbp, [rcx + 48]")
  (emit "  mov rsp, [rcx + 56]")
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
;                 0
;                 (fx+ n (apply fac (fxsub1 n)))))))
;      (apply fac 4000)))
; (emit-program '(cons (cons 1 2) (cons 3 4)))
; (emit-program '(vector-ref (make-vector 5 #t) 4))
(emit-program '(let*
                 ((x (make-vector 5 0))
                  (y (vector-set! x 0 0))
                  (z (vector-set! x 1 1))
                  (w (vector-set! x 2 2))
                  (v (vector-set! x 3 3))
                  (u (vector-set! x 4 4)))
                  (vector-ref x 3)))
