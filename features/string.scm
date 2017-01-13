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
