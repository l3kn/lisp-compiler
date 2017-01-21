(define rax (list 'reg "rax"))
(define rbx (list 'reg "rbx"))
(define rcx (list 'reg "rcx"))
(define rdx (list 'reg "rdx"))
(define rsi (list 'reg "rsi"))
(define rdi (list 'reg "rdi"))
(define rbp (list 'reg "rbp"))
(define rsp (list 'reg "rsp"))
(define r8 (list 'reg "r8"))
(define r9 (list 'reg "r9"))
(define r10 (list 'reg "r10"))
(define r11 (list 'reg "r11"))
(define r12 (list 'reg "r12"))
(define r13 (list 'reg "r13"))
(define r14 (list 'reg "r14"))
(define r15 (list 'reg "r15"))

(define (reg? obj)
  (tagged-list? obj 'reg))

(define (mem? obj)
  (string? obj))

(define (const? obj)
  (number? obj))

(define (label? obj)
  (symbol? obj))

(define reg-name cadr)
(define const-value cadr)

(define (offset reg n)
  (cond
    ((< n 0) (show "[" (reg-name reg) "-" (- n) "]"))
    ((= n 0) (show "[" (reg-name reg) "]"))
    ((> n 0) (show "[" (reg-name reg) "+" n "]"))))

(define (type arg)
  (cond
    ((mem? arg) 'mem)
    ((reg? arg) 'reg)
    ((const? arg) 'const)
    ((label? arg) 'label)
    (else (error "Invalid arg: " arg))))

(define (format-arg arg)
  (cond
    ((mem? arg) arg)
    ((reg? arg) (reg-name arg))
    ((const? arg) (show "QWORD PTR " arg))
    (else (error "Invalid arg: " arg))))

(define (make-asm-op name arity arg-types)
  (lambda args
    (let ((arity2 (length args))
          (arg-type (map type args)))
      (cond
        ((< arity arity2)
         (error "Too many arguments for op " name ": expected " arity ", got " arity2)) 
        ((> arity arity2)
         (error "Too few arguments for op " name ": expected " arity ", got " arity2)) 
        ((member arg-type arg-types)
         (case arity
           ((0) (show "  " name))
           ((1) (show "  " name " " (format-arg (car args))))
           ((2) (show "  " name " " (format-arg (car args))
                                ", " (format-arg (cadr args))))
           ((3) (show "  " name " " (format-arg (car args))
                                ", " (format-arg (cadr args))
                                ", " (format-arg (caddr args))))
           (else (error "Invalid arity: " arity ", must be 1, 2 or 3"))))
        (else
          (error "Invalid argument type: " arg-type ", must be one of " arg-types))))))

(define push
  (make-asm-op 'push 1 '((reg) (const) (mem))))
(define pop
  (make-asm-op 'pop 1 '((reg) (mem))))

(define add
  (make-asm-op 'add 2 '((reg reg)
                        (reg const)
                        (reg mem)
                        (mem reg)
                        (mem const))))
(define sub
  (make-asm-op 'sub 2 '((reg reg)
                        (reg const)
                        (reg mem)
                        (mem reg)
                        (mem const))))

(define mov
  (make-asm-op 'mov 2 '((reg reg)
                        (reg mem)
                        (mem reg)
                        (reg const)
                        (mem const))))

(define ret
  (make-asm-op 'ret 0 '(())))

; Adding mem here is a bit cheaty,
; because in some cases we use labels like 'foo
; and in other cases ones like "foo".
; label? = symbol? and mem? = string?
; but bc. both just get appended to the op,
; it doesn't make a big difference

(define call
  (make-asm-op 'call 1 '((label) (mem))))

(define je
  (make-asm-op 'call 1 '((label) (mem))))

(define jmp
  (make-asm-op 'call 1 '((label) (mem))))

(define binop-types
  '((reg reg)
    (reg mem)
    (reg const)
    (mem reg)
    (mem const)))

(define (make-asm-binop name)
  (make-asm-op name 2 binop-types))


(define cmp (make-asm-binop 'cmp))

(define sete (make-asm-op 'sete 1 '((reg) (mem))))
(define setne (make-asm-op 'setne 1 '((reg) (mem))))

(define shl (make-asm-binop 'shl))
(define shr (make-asm-binop 'shr))
(define sal (make-asm-binop 'sal))
(define sar (make-asm-binop 'sar))
(define and_ (make-asm-binop 'and))
(define or_ (make-asm-binop 'or))
(define xor (make-asm-binop 'xor))
