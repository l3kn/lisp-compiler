; (register-primitive 'sys-write
;       (lambda (stack-index env args)
;         (let ((arg (car args)))
;           (emit-expr stack-index env arg #f)
;           (emit "  xor rax, " string-tag)

;           ; Now rax should contain a pointer to the heap
;           ; 8 bytes size | 1 byte char1 | 1 byte char2 | ...
;           (emit "  mov rdi, QWORD PTR 1") ; file descriptor
;           (emit "  mov rdx, [rax]") ; size = first 8 bytes of the string
;           (emit "  add rax, 8")
;           (emit "  mov rsi, rax") ; buffer = *string + 8
;           (emit "  mov rax, QWORD PTR 1") ; syscall #1 = sys_write
;           (emit "  syscall"))))

; (define (emit-sys-write-byte stack-index env byte)
;   (emit "  mov r11, rsp")
;   (emit "  sub r11, " stack-index)
;   (emit "  mov [r11], BYTE PTR " byte)

;   (emit "  mov rdi, QWORD PTR 1") ; file descriptor
;   (emit "  mov rdx, 1")

;   (emit "  mov rsi, r11") ; buffer = bottom of the stack
;   (emit "  mov rax, QWORD PTR 1") ; syscall #1 = sys_write
;   (emit "  syscall"))
