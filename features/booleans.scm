(define bool_false #b00101111)
(define bool_true  #b01101111)

; (register-raw-predicate 'boolean?
;       (lambda (stack-index env args)
;         (let ((arg (car args)))
;           (emit-expr stack-index env arg #f)
;           (print "  and rax, " bool_mask)
;           (print "  cmp rax, " bool_tag))))

; (register-raw-predicate 'not
;       (lambda (stack-index env args)
;         (let ((arg (car args)))
;           (emit-expr stack-index env arg #f)
;           (print "  cmp rax, " (immediate-rep #f)))))
