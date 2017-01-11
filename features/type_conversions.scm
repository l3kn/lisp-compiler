(register-primitive 'char->fixnum
      (lambda (stack-index env args)
        (let ((arg (car args)))
          (emit-expr stack-index env arg)
          (print "  shr rax, " (- char_shift fixnum_shift)))))

(register-primitive 'fixnum->char
      (lambda (stack-index env args)
        (let ((arg (car args)))
          (emit-expr stack-index env arg)
          (print "  shl rax, " (- char_shift fixnum_shift))
          (print "  or  rax, " char_tag))))
