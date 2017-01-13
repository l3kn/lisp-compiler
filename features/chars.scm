(define char_shift 8)
(define char_mask #b11111111)
(define char_tag #b00001111)

(register-raw-predicate 'char?
      (lambda (stack-index env args)
        (let ((arg (car args)))
          (emit-expr stack-index env arg #f)
          (print "  and rax, " char_mask)
          (print "  cmp rax, " char_tag))))

(register-primitive 'char=?
      (lambda (stack-index env args)
        (emit-comparison stack-index env (car args) (cadr args))
        (emit-test stack-index env "je")))

(register-primitive 'char<?
      (lambda (stack-index env args)
        (emit-comparison stack-index env (car args) (cadr args))
        (emit-test stack-index env "jl")))
(register-primitive 'char<=?
      (lambda (stack-index env args)
        (emit-comparison stack-index env (car args) (cadr args))
        (emit-test stack-index env "jlt")))

(register-primitive 'char>?
      (lambda (stack-index env args)
        (emit-comparison stack-index env (car args) (cadr args))
        (emit-test stack-index env "jg")))
(register-primitive 'char>=?
      (lambda (stack-index env args)
        (emit-comparison stack-index env (car args) (cadr args))
        (emit-test stack-index env "jgt")))
