(define vector_mask #b111)
(define vector_tag #b101)

(register-raw-predicate 'vector?
      (lambda (stack-index env args)
        (let ((arg (car args)))
          (emit-expr stack-index env arg)
          (emit "  and rax, " vector_mask)
          (emit "  cmp rax, " vector_tag))))

(register-primitive 'make-vector
      (lambda (stack-index env args)
        (let ((size (car args))
              (default (cadr args))
              (loop-label (unique-label "loop"))
              (end-label (unique-label "end")))
          ; Evaluate the size
          (emit "  mov r10, rbp")
          (emit "  mov r11, rbp")
          (emit-expr stack-index env size)
          ; Up till now, the size was stored in the upper 30 bits
          ; of rax, the lower 2 bits are 00b.
          ; We need size * 8 (size in bytes),
          ; so all thats left to do is to multiply by 2
          (emit "  shl rax, 1")
          (emit "  add rbp, rax")
          (emit "  mov r12, rax")
          (emit "  shr r12, 3")
          (emit-expr stack-index env default)
          ; Now the size is stored in r12,
          ; the base pointer of the vector is stored in r10 and r11
          ; and the default value is stored in rax
          (emit "  mov [r10], r12")
          (emit "  add r10, 8")
          (emit loop-label ":")
          (emit "  cmp r12, 0")
          (emit "  je " end-label)
          (emit "  mov [r10], rax")
          (emit "  add r10, 8")
          (emit "  sub r12, 1")
          (emit "  jmp " loop-label)
          (emit end-label ":")
          (emit "  mov rax, r11")
          (emit "  or rax, " vector_tag)
          )))

(register-primitive 'vector-ref
      (lambda (stack-index env args)
        (let ((vec (car args))
              (index (cadr args)))
          (emit-expr stack-index env vec)
          (emit "  mov r10, rax")
          ; Fill tag field w/ 0s
          (emit "  shr r10, 3")
          (emit "  shl r10, 3")
          ; TODO: Check if index is in range
          (emit-expr stack-index env index)
          (emit "  add rax, 1")
          (emit "  shl rax, 3")
          (emit "  add rax, r10")
          (emit "  mov rax, [rax]"))))
