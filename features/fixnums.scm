(define fixnum_shift 2)
(define fixnum_mask #b11)
(define fixnum_tag #b00)

(define fixnum_bits (- (* wordsize 8) fixnum_shift))
(define fixnum_lower (- (expt 2 (- fixnum_bits 1))))
(define fixnum_upper (sub1 (expt 2 (- fixnum_bits 1))))

(define (fixnum? x)
  (and (integer? x)
       (<= fixnum_lower x fixnum_upper)))

(register-raw-predicate 'fxzero?
      (lambda (stack-index env args)
        (let ((arg (car args)))
          (emit-expr stack-index env arg #f)
          (print "  cmp rax, " (immediate-rep 0)))))

(register-raw-predicate 'fixnum?
      (lambda (stack-index env args)
        (let ((arg (car args)))
          (emit-expr stack-index env arg #f)
          (print "  and rax, " fixnum_mask)
          (print "  cmp rax, " fixnum_tag))))

(register-primitive 'fxadd1
      (lambda (stack-index env args)
        (let ((arg (car args)))
          (emit-expr stack-index env arg #f)
          (print "  add rax, " (immediate-rep 1)))))
(register-primitive 'fxsub1
      (lambda (stack-index env args)
        (let ((arg (car args)))
          (emit-expr stack-index env arg #f)
          (print "  sub rax, " (immediate-rep 1)))))

(register-primitive 'fxlognot
      (lambda (stack-index env args)
        (let ((arg (car args)))
          (emit-expr stack-index env arg #f)
          (print "  shr rax, " fixnum_shift)
          (print "  not rax")
          (print "  shl rax, " fixnum_shift))))

(register-primitive 'fx+
      (lambda (stack-index env args)
        (emit-binop stack-index env (car args) (cadr args) "add")))
(register-primitive 'fx-
      (lambda (stack-index env args)
        ; Swap the arguments around,
        ; we want to evaluate the cadr first,
        ; put it on the stack,
        ; evaluate the car to rax
        ; and then sub the value of the cadr from it
        (emit-binop stack-index env (cadr args) (car args) "sub")))
(register-primitive 'fxlogand
      (lambda (stack-index env args)
        (emit-binop stack-index env (car args) (cadr args) "and")))
(register-primitive 'fxlogor
      (lambda (stack-index env args)
        (emit-binop stack-index env (car args) (cadr args) "or")))

(register-primitive 'fx*
      (lambda (stack-index env args)
        (let ((arg1 (car args))
              (arg2 (cadr args)))
          (emit-expr stack-index env arg2)
          (print "  shr rax, " fixnum_shift)
          (print "  mov [rsp - " stack-index "], rax")
          (emit-expr (next-stack-index stack-index) env arg1)
          (print "  shr rax, " fixnum_shift)
          (print "  imul rax, [rsp - " stack-index "]")
          (print "  shl rax, " fixnum_shift))))
(register-primitive 'fx/
      (lambda (stack-index env args)
        (let ((arg1 (car args))
              (arg2 (cadr args)))
          (emit-expr stack-index env arg2)
          (print "  mov [rsp - " stack-index "], rax")
          (emit-expr (next-stack-index stack-index) env arg1)
          (print "  idiv rax, [rsp - " stack-index "]")
          (print "  shl rax, " fixnum_shift))))
(register-primitive 'fx=?
      (lambda (stack-index env args)
        (emit-comparison stack-index env (car args) (cadr args))
        (emit-test stack-index env "je")))
(register-primitive 'fx<?
      (lambda (stack-index env args)
        (emit-comparison stack-index env (car args) (cadr args))
        (emit-test stack-index env "jl")))
(register-primitive 'fx<=?
      (lambda (stack-index env args)
        (emit-comparison stack-index env (car args) (cadr args))
        (emit-test stack-index env "jlt")))
(register-primitive 'fx>?
      (lambda (stack-index env args)
        (emit-comparison stack-index env (car args) (cadr args))
        (emit-test stack-index env "jg")))
(register-primitive 'fx>=?
      (lambda (stack-index env args)
        (emit-comparison stack-index env (car args) (cadr args))
        (emit-test stack-index env "jgt")))
