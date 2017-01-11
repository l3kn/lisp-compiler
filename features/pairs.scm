(define (cons? expr) (tagged-list? expr 'cons))

(register-raw-predicate 'pair?
      (lambda (stack-index env args)
        (let ((arg (car args)))
          (emit-expr stack-index env arg)
          (print "  and rax, " pair_mask)
          (print "  cmp rax, " pair_tag))))

(register-primitive 'cons
  (lambda (emit-cons stack-index env expr)
    ; Increment the base pointer first,
    ; so (cons (cons 1 2) (cons 3 4)) won't mess it up
    (emit "  add rbp, 16") 
    (emit-comment "cons, car")

    ; Store the old heap pointer
    (print "  mov [rsp - " stack-index "], rbp")
    ; Evaluate the expression
    (emit-expr (next-stack-index stack-index) env (cadr expr))
    ; Store the new heap pointer
    (print "  mov [rsp - " (next-stack-index stack-index) "], rbp")
    ; Load the old heap pointer and store the result at the correct location
    (print "  mov rbp, [rsp - " stack-index "]")
    (emit "  mov [rbp - 16], rax")
    ; Restore the new heap pointer
    (print "  mov rbp, [rsp - " (next-stack-index stack-index) "]")

    (emit-expr (next-stack-index stack-index) env (caddr expr))
    ; Store the new heap pointer
    (print "  mov [rsp - " (next-stack-index stack-index) "], rbp")
    ; Load the old heap pointer and store the result at the correct location
    (print "  mov rbp, [rsp - " stack-index "]")
    (emit "  mov [rbp - 8], rax")
    ; Restore the new heap pointer
    (print "  mov rbp, [rsp - " (next-stack-index stack-index) "]")
    (print "  mov rax, [rsp - " stack-index "]")
    (emit "  sub rax, 16")
    (print "  or  rax, " pair_tag)
  )
)

(register-primitive 'car
      (lambda (stack-index env args)
        (emit-expr stack-index env (car args))
        ; Remove the tag of the pair value
        ; to get the address
        (print "  and al, " #b11111000)
        (print "  mov rax, [rax]")))

(register-primitive 'cdr
      (lambda (stack-index env args)
        (emit-expr stack-index env (car args))
        ; Remove the tag of the pair value
        ; to get the address
        (print "  and al, " #b11111000)
        (print "  mov rax, [rax + 8]")))
