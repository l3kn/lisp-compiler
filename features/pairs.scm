(define pair_mask #b111)
(define pair_tag #b001)

(register-raw-predicate 'pair?
      (lambda (stack-index env args)
        (let ((arg (car args)))
          (emit-expr stack-index env arg #f)
          (emit "  and rax, " pair_mask)
          (emit "  cmp rax, " pair_tag))))

(register-primitive 'cons
  (lambda (stack-index env args)
    ; Increment the base pointer first,
    ; so (cons (cons 1 2) (cons 3 4)) won't mess it up.
    ; car-addr is in r10, cdr-addr in r11
    (emit "  mov [rsp - " stack-index "], rbx")
    (emit "  add rbx, 8") 
    (emit "  mov [rsp - " (next-stack-index stack-index) "], rbx")
    (emit "  add rbx, 8") 
    (emit-comment "cons, car")
    (emit-expr (next-stack-index-n stack-index 2) env (car args))
    (emit "  mov r10, [rsp - " stack-index "]")
    (emit "  mov [r10], rax")
    (emit-comment "cons, cdr")
    (emit-expr (next-stack-index-n stack-index 2) env (cadr args))
    (emit "  mov r10, [rsp - " (next-stack-index stack-index) "]")
    (emit "  mov [r10], rax")
    ; Restore the new heap pointer
    (emit "  mov rax, [rsp - " stack-index "]")
    (emit "  or rax, " pair_tag)
  )
)

(register-primitive 'car
      (lambda (stack-index env args)
        (emit-expr stack-index env (car args))
        ; Remove the tag of the pair value
        ; to get the address
        (emit "  and al, " #b11111000)
        (emit "  mov rax, [rax]")))

(register-primitive 'cdr
      (lambda (stack-index env args)
        (emit-expr stack-index env (car args))
        ; Remove the tag of the pair value
        ; to get the address
        (emit "  and al, " #b11111000)
        (emit "  mov rax, [rax + 8]")))
