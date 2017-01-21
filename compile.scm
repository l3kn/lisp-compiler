(define wordsize 8)

(include "environment.scm")
(include "asm.scm")
(include "syntax/derived/and.scm")
(include "syntax/derived/or.scm")
(include "syntax/if.scm")
(include "syntax/begin.scm")
(include "syntax/let.scm")
(include "primitives.scm")
(include "procedures.scm")
(include "features/booleans.scm")
; (include "features/pairs.scm")
; (include "features/vectors.scm")
(include "features/fixnums.scm")
; (include "features/chars.scm")
(include "features/type_conversions.scm")
(include "features/system.scm")
; (include "features/string.scm")

(define empty_list #b00111111)

(define (string-join lst) (foldl string-append "" lst))

(define (show . args) (string-join (map ->string args)))

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
        (emit-stack-load_ location)
        (else (error "Reference to unbound variable: " var)))))

(define (emit-stack-save stack-index env expr tail)
  (if (immediate? expr)
    (emit (push (immediate-rep expr)))
    (begin
      (emit-expr stack-index env expr tail)
      (emit (push rax)))))

(define (emit-stack-load stack-index)
  (emit (pop rax)))

(define (emit-stack-save_ stack-index env expr tail)
  (if (immediate? expr)
    (emit (mov (offset rsp (- stack-index)) (immediate-rep expr)))
    (begin
      (emit-expr stack-index env expr tail)
      (emit (mov (offset rsp (- stack-index)) rax)))))

(define (emit-stack-load_ stack-index)
  (emit (mov rax (offset rsp (- stack-index)))))

(define (emit . output)
  (apply print output))

(define (emit-immediate expr)
  (emit (mov rax (immediate-rep expr))))

(define (emit-expr stack-index env expr tail)
  (cond
    ((immediate? expr)
     (emit-immediate expr))
    ((if? expr)
     (emit-if stack-index env expr tail))
    ((begin? expr)
     (emit-begin stack-index env expr tail))
    ((and? expr) (emit-expr stack-index env (and->if expr) tail))
    ((or? expr) (emit-expr stack-index env (or->if expr) tail))
    ((let? expr) (emit-let stack-index env expr tail))
    ((let*? expr) (emit-let* stack-index env expr tail))
    ((apply? expr) (emit-apply stack-index env expr tail))
    ((string? expr) (emit-string stack-index env expr))
    ; ((primitive? expr)
    ;  (emit-comment (car expr))
    ;  ((lookup-primitive (car expr)) stack-index env (cdr expr))
    ;  (emit-comment "end " (car expr)))
    ; ((predicate? expr)
    ;    (let ((raw-predicate (lookup-raw-predicate (car expr)))
    ;          (true-label (unique-label "true"))
    ;          (end-label (unique-label "end")))
    ;      (emit-comment (car expr))
    ;      (raw-predicate stack-index env (cdr expr))
    ;      (emit (je true-label))
    ;      (emit-immediate #f)
    ;      (emit (jmp end-label))
    ;      (emit true-label ":")
    ;      (emit-immediate #t)
    ;      (emit end-label ":")
    ;      (emit-comment "end " (car expr))))
    ((list? expr)
     (let ((name (car expr))
           (args (cdr expr)))
       (emit-arguments stack-index env args tail)
       (emit (sub rsp stack-index))
       (emit (call (escape name)))
       (emit (add rsp stack-index))))
    ((variable? expr)
     (emit-variable-ref env expr tail))
    (else
      (error "Unknown expression: " expr))))

(define calling-convention
  (list rdi rsi rdx rcx r8 r9))

(define (emit-arguments stack-index env args tail)
  (if (> (length args) (length calling-convention))
      (error "Functions can have max. 6 arguments"))
  (define (helper args registers)
    (if (not (null? args))
      (begin
         (if (immediate? (car args))
             (emit (mov (car registers) (immediate-rep (car args))))
             (begin
               (emit-expr stack-index env (car args) tail)
               (emit (mov (car registers) rax))))
         (helper (cdr args) (cdr registers)))))
  (helper args calling-convention))

(define (emit-program expr)
  (emit "  .intel_syntax noprefix")
  (emit "  .text")
  (if (letrec? expr)
      (emit-letrec expr #f)
      (emit-scheme-entry expr empty-env #f)))

(define primitives '())

(define (register-primitive name code)
  (let ((old primitives))
    (set! primitives (cons (list name code) old))))

(register-primitive 'fx+
  (list
    (mov rax rdi)
    (add rax rsi)
    (ret)
    ))
(register-primitive 'fx-
  (list
    (mov rax rdi)
    (sub rax rsi)
    (ret)
    ))
(register-primitive 'fxzero?
  (list
    (xor rax rax) ; reset rax
    (cmp rdi (immediate-rep 0))
    ; bool_false = #b00101111
    ; bool_true  = #b01101111
    (sete '(reg al))
    ; rax = 0....01 or
    ; rax = 0....00
    (sal rax 6)
    ; rax = 0....01000000 or
    ; rax = 0....00000000
    (or_ rax (immediate-rep #f))
    ; rax = 0....01101111 or
    ; rax = 0....00101111
    (ret)))
(register-primitive 'fxadd1
  (list
    (mov rax rdi)
    (add rax (immediate-rep 1))
    (ret)))
(register-primitive 'fxsub1
  (list
    (mov rax rdi)
    (sub rax (immediate-rep 1))
    (ret)))
(register-primitive 'not
  (list
    (xor rax rax) ; reset rax
    (cmp rdi (immediate-rep #t))
    (setne '(reg al))
    (sal rax 6)
    (or_ rax (immediate-rep #f))
    (ret)))

(define (emit-primitives)
  (for-each (lambda (entry)
              (emit-function-header (escape (car entry)))
              (for-each emit (cadr entry)))
            primitives))

(define (emit-scheme-entry expr env tail)
  (emit-function-header "scheme_body")
  (emit-expr wordsize env expr tail)
  (emit (ret)))

; Scheme entry:
; (emit "  ret")
; (print "  .text")
; (emit-function-header "scheme_entry")
; ; 64bit calling convention:
; ; RDI, RSI, RDX, RCX (R10 in the Linux kernel interface), R8, and R9
; ; ctxt: RDI
; ; stack_base: RSI -> RSP, RBP
; ; heap: RDX -> RBX
; ; TODO: Don't use ctxt, just push all regs
; (emit-comment "store register contents in ctxt")

; ; Store old stack pointers in unused volative registers
; (emit (mov rcx rsp))
; (emit (mov rdx rbp))

; ; Load the new rsp and rbp
; (emit (mov rsp rsi))
; (emit (mov rbp rsi))

; ; Store the old registers on the stack
; (emit (push rcx)) ; old rsp
; (emit (push rdx)) ; old rbp
; (emit (push rbx))
; (emit (push rsi))
; (emit (push rdi))

; ; Execute the main body
; ; (emit "  mov rbx, rdx")
; (emit "  call scheme_body")

; ; Restore the old registers from the stack
; (emit (pop rdi))
; (emit (pop rsi))
; (emit (pop rbx))
; (emit (pop rbp))
; (emit (pop rsp))
; ; (emit "  mov rax, 60")
; ; (emit "  mov rdi, 0")
; ; (emit "  syscall")
; (emit (ret)))

(define (emit-function-header name)
  (emit "  .globl " name)
  (emit "  .type " name ", @function")
  (emit name ":"))

(define (escape str)
  (let ((parts (map ->string (string->list (->string str)))))
    (string-join
      (cons "prim_"
        (map
          (lambda (part)
            (cond
              ((equal? part "+") "_plus_")
              ((equal? part "-") "_minus_")
              ((equal? part ">") "_greater_")
              ((equal? part "<") "_less_")
              ((equal? part "=") "_equal_")
              ((equal? part "*") "_times_")
              ((equal? part "?") "_questionmark_")
              (else part)))
          parts)))))

; (emit-program '(boolean? #\A))
; (emit-program '(char->fixnum #\A))
; (emit-program '(fixnum->char 65))
; (emit-program '(not #t))
; (emit-program '(fxlognot -11))
; (emit-program 10)
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
;      (apply sum 0 0)))
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
; (emit-program '(fx/ 10 5))
; (emit-program '(sys-write "hello world"))
; (emit-program '(string-length (fixnum->string 123)))
; (emit-program '(sys-write (fixnum->string 1234)))
; (emit-program '(display "hello"))
; (emit-program '(fixnum->string 0))
