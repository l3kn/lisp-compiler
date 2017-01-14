(define (emit-string stack-index env expr)
  (let ((len (string-length expr)))
    (define (emit-chars offset index)
      (if (< index len)
          (begin
            (emit "  mov [rbp + " offset "], QWORD PTR " (char->integer (string-ref expr index)))
            (emit-chars (add1 offset) (add1 index)))))
    (emit "  mov [rbp], QWORD PTR " len)
    (emit-chars 8 0)
    (emit "  mov rax, rbp")
    (emit "  add rbp, 8")
    ; Align heap pointer at 8 byte boundaries
    (emit "  add rbp, " (* (fx/ (sub1 (+ len wordsize)) wordsize) wordsize))))

(register-primitive 'string-length
  (lambda (stack-index env args)
    (let ((arg (car args)))
      (emit-expr stack-index env arg #f)
      (emit "  mov rax, [rax]")
      (emit "  shl rax, 2"))))

(register-primitive 'fixnum->string
  (lambda (stack-index env args)
    (let ((arg (car args))
          (digits-loop-label (unique-label "digit_loop"))
          (digits-end-label (unique-label "digit_end"))
          (loop-label (unique-label "loop")))
      (emit-expr stack-index env arg #f)
      (emit "  shr rax, 2") ; Remove the fixnum tag
      ; r12: digits, r13: start heap pointer, r14: stack pointer for digits
      (emit "  mov r12, 0")
      (emit "  mov r13, rbp")
      (emit "  mov r14, rsp")
      (emit "  sub r14, " stack-index)
      ; TODO: handle negative numbers

      ; Split the fixnume into digits,
      ; r12: number of digits, r14: pointer top of stack w/ digits in reverse order
      (emit "  mov rbx, 10") ; Always use 10 as divisor
      (emit-label digits-loop-label)
      ; (emit "  jz " digits-end-label)
      (emit "  cqo") ; sign-extend rax to rdx:rax
      (emit "  idiv rbx") ; => rax: quotient, rdx: remainder
      (emit "  add rdx, 48") ; ascii "0" = 48
      (emit "  mov [r14], rdx")
      (emit "  sub r14, 8") ; the stack grows from high to low
      (emit "  add r12, 1")
      (emit "  cmp rax, 0")
      (emit "  jg " digits-loop-label)
      (emit-label digits-end-label)

      ; Store the size of the string
      (emit "  mov [rbp], r12")
      (emit "  add rbp, 8")

      ; Read the digits from the stack (in reverse order)
      ; and store them on the heap as ascii bytes
      ; r11: pointer to the last digit
      (emit "  mov rcx, r12")
      (emit-label loop-label)
      (emit "  add r14, 8") ; The stack grows from high to low
      (emit "  mov r11, QWORD PTR [r14]") ; TODO: is this already to high?
      (emit "  mov [rbp], r11") ; store the lowest byte of r11 on the heap
      (emit "  add rbp, 1")
      (emit "  loop " loop-label)

      ; align the heap pointer at 8 byte boundaries
      (emit "  and r12, 7") ; r12 = r12 % 8
      (emit "  add rbp, 8")
      (emit "  sub rbp, r12") ; rbp += 8 - (digits % 8)
      ; return the pointer to the start of the number
      (emit "  mov rax, r13"))))
