(define string-tag #b110)

(define (emit-string stack-index env expr)
  (let ((len (string-length expr)))
    (define (emit-chars offset index)
      (if (< index len)
          (begin
            (emit "  mov [rbx + " offset "], BYTE PTR " (char->integer (string-ref expr index)))
            (emit-chars (add1 offset) (add1 index)))))
    (emit "  mov [rbx], QWORD PTR " len)
    (emit-chars 8 0)
    (emit "  mov rax, rbx")
    (emit "  add rax, " string-tag)
    (emit "  add rbx, 8")
    ; Align heap pointer at 8 byte boundaries
    (emit "  add rbx, " (* (fx/ (sub1 (+ len wordsize)) wordsize) wordsize))))

(register-primitive 'string-length
  (lambda (stack-index env args)
    (let ((arg (car args)))
      (emit-expr stack-index env arg #f)
      (emit "  xor rax, " string-tag)
      (emit "  mov rax, [rax]")
      (emit "  shl rax, 2"))))

(register-primitive 'display
  (lambda (stack-index env args)
    (let ((arg (car args))
          (string-label (unique-label "string"))
          (end-label (unique-label "end"))
          )
      (emit-expr stack-index env arg #f)
      (emit "  mov r11, rax")
      (emit "  and r11, 7") ; Just keep the tag bits
      (emit "  cmp r11, " string-tag)
      (emit "  je " string-label)
      (emit "  jmp " end-label)
      (emit-label string-label)
      (emit-sys-write-byte stack-index env 34)
      (emit-sys-write-byte stack-index env 34)
      (emit-label end-label))))

(register-primitive 'fixnum->string
  (lambda (stack-index env args)
    (let ((arg (car args))
          (digits-loop-label (unique-label "digit_loop"))
          (negate-end-label (unique-label "negate_end"))
          (sign-end-label (unique-label "sign_end"))
          (loop-label (unique-label "loop")))
      (emit-expr stack-index env arg #f)
      ; r11: signed?, r12: digits, r13: start heap pointer, r14: stack pointer for digits
      (emit "  mov r11, 0")
      (emit "  mov r12, 0")
      (emit "  mov r13, rbx")
      (emit "  mov r14, rsp")
      (emit "  sub r14, " stack-index)

      ; Negate the number if it is negative
      (emit "  cmp rax, 0")
      (emit "  jg " negate-end-label)
      (emit "  mov r11, 1") 
      (emit "  neg rax")
      (emit-label negate-end-label)

      (emit "  shr rax, 2") ; Remove the fixnum tag
      ; Split the fixnume into digits,
      ; r12: number of digits, r14: pointer top of stack w/ digits in reverse order
      (emit "  mov rbx, 10") ; Always use 10 as divisor
      (emit-label digits-loop-label)
      (emit "  cqo") ; sign-extend rax to rdx:rax
      (emit "  idiv rbx") ; => rax: quotient, rdx: remainder
      (emit "  add rdx, 48") ; ascii "0" = 48
      (emit "  mov [r14], rdx")
      (emit "  sub r14, 8") ; the stack grows from high to low
      (emit "  add r12, 1")
      (emit "  cmp rax, 0")
      (emit "  jg " digits-loop-label)

      ; If r11 is set to 1, add a "-" in front of the string
      (emit "  cmp r11, 1")
      (emit "  jne " sign-end-label)
      (emit "  mov [r14], QWORD PTR 45")
      (emit "  sub r14, 8")
      (emit "  add r12, 1")
      (emit-label sign-end-label)

      ; Store the size of the string
      (emit "  mov [rbx], r12")
      (emit "  add rbx, 8")

      ; Read the digits from the stack (in reverse order)
      ; and store them on the heap as ascii bytes
      ; r11: pointer to the last digit
      (emit "  mov rcx, r12")
      (emit-label loop-label)
      (emit "  add r14, 8") ; The stack grows from high to low
      (emit "  mov r11, QWORD PTR [r14]")
      (emit "  mov [rbx], r11") ; store the lowest byte of r11 on the heap
      (emit "  add rbx, 1")
      (emit "  loop " loop-label)

      ; align the heap pointer at 8 byte boundaries
      (emit "  and r12, 7") ; r12 = r12 % 8
      (emit "  add rbx, 8")
      (emit "  sub rbx, r12") ; rbx += 8 - (digits % 8)
      ; return the pointer to the start of the number
      (emit "  mov rax, r13")
      (emit "  add rax, " string-tag)
      )))
