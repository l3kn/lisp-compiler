(include "constants.scm")
(include "environment.scm")
(include "syntax/derived/and.scm")
(include "syntax/derived/or.scm")
(include "syntax/if.scm")
(include "syntax/let.scm")
(include "procedures.scm")
(include "primitives.scm")

(define (tagged-list? expr tag)
  (and (pair? expr)
       (eq? (car expr) tag)))

(define (fixnum? x)
  (and (integer? x)
       (<= fixnum_lower x fixnum_upper)))

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
  (apply print (cons "  # " args)))

(define (next-stack-index stack-index) (+ stack-index wordsize))
(define (prev-stack-index stack-index) (- stack-index wordsize))

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
    (print "  mov [rsp - " stack-index "], QWORD PTR " (immediate-rep expr))
    (begin
      (emit-expr stack-index env expr)
      (print "  mov [rsp - " stack-index "], rax"))))

(define (emit-stack-load stack-index)
  (print "  mov rax, [rsp - " stack-index "]"))

(define (emit output)
  (print output))

(define (emit-immediate expr)
  (print "  mov rax, QWORD PTR " (immediate-rep expr)))

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
         (print "  je " true-label)
         (emit-immediate #f)
         (print "  jmp " end-label)
         (print true-label ":")
         (emit-immediate #t)
         (print end-label ":")
         (emit-comment "end " (car expr))))
    ((variable? expr)
     (emit-variable-ref env expr))
    (else
      (error "Unknown expression: " expr))))

(define (emit-tail-expr stack-index env expr)
  (cond
    ((immediate? expr)
     (emit-immediate expr)
     (print "  ret"))
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
     (print " ret"))
    ((predicate? expr)
       (let ((raw-predicate (lookup-raw-predicate (car expr)))
             (true-label (unique-label "true"))
             (end-label (unique-label "end")))
         (emit-comment (car expr))
         (raw-predicate stack-index env (cdr expr))
         (print "  je " true-label)
         (emit-immediate #f)
         (print "  jmp " end-label)
         (print true-label ":")
         (emit-immediate #t)
         (print end-label ":")
         (emit-comment "end " (car expr))))
    ((variable? expr)
     (emit-tail-variable-ref env expr)
     (print "  ret"))
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
  ; Store stack pointer
  (emit "  mov rcx, rsp")
  ; Load new stack pointer,
  ; 64bit calling convention:
  ; RDI, RSI, RDX, RCX (R10 in the Linux kernel interface), R8, and R9
  (emit "  mov rsp, rdi")
  (emit "  call scheme_body")
  ; Restore stack pointer
  (emit "  mov rsp, rcx")
  (emit "  ret"))

(define (emit-tail-scheme-entry expr env)
  (emit-function-header "scheme_body")
  (emit-tail-expr wordsize env expr)
  (emit "  ret")
  (emit "  .text")
  (emit-function-header "scheme_entry")
  ; Store stack pointer
  (emit "  mov rcx, rsp")
  ; Load new stack pointer,
  ; 64bit calling convention:
  ; RDI, RSI, RDX, RCX (R10 in the Linux kernel interface), R8, and R9
  (emit "  mov rsp, rdi")
  (emit "  call scheme_body")
  ; Restore stack pointer
  (emit "  mov rsp, rcx")
  (emit "  ret"))

(define (emit-function-header name)
  (print "  .globl " name)
  (print "  .type " name ", @function")
  (print name ":"))

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
(emit-program
  '(letrec
     ((fac (lambda (n)
            (if (fxzero? n)
                0
                (fx+ n (apply fac (fxsub1 n)))))))
     (apply fac 4000)))
