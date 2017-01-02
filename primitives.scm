(define primitives
  (list 
    (list 'fxadd1
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  add eax, " (immediate-rep 1)))))
    (list 'fxsub1
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  sub eax, " (immediate-rep 1)))))
    (list 'fxzero?
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  cmp eax, " (immediate-rep 0))
              (print "  je true")
              (print "  mov eax, " (immediate-rep #f))
              (print "  jmp end")
              (print "true:")
              (print "  mov eax, " (immediate-rep #t))
              (print "end:"))))
    (list 'fxlognot
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  shr eax, " fixnum_shift)
              (print "  not eax")
              (print "  shl eax, " fixnum_shift))))
    (list 'fixnum?
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  and eax, " fixnum_mask)
              (print "  cmp eax, " fixnum_tag)
              (print "  je true")
              (print "  mov eax, " (immediate-rep #f))
              (print "  jmp end")
              (print "true:")
              (print "  mov eax, " (immediate-rep #t))
              (print "end:"))))
    (list 'char?
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  and eax, " char_mask)
              (print "  cmp eax, " char_tag)
              (print "  je true")
              (print "  mov eax, " (immediate-rep #f))
              (print "  jmp end")
              (print "true:")
              (print "  mov eax, " (immediate-rep #t))
              (print "end:"))))
    (list 'boolean?
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  cmp eax, " (immediate-rep #t))
              (print "  je true")
              (print "  cmp eax, " (immediate-rep #f))
              (print "  je true")
              (print "  mov eax, " (immediate-rep #f))
              (print "  jmp end")
              (print "true:")
              (print "  mov eax, " (immediate-rep #t))
              (print "end:"))))
    (list 'null?
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  cmp eax, " (immediate-rep '()))
              (print "  je true")
              (print "  mov eax, " (immediate-rep #f))
              (print "  jmp end")
              (print "true:")
              (print "  mov eax, " (immediate-rep #t))
              (print "end:"))))
    (list 'char->fixnum
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  shr eax, " (- char_shift fixnum_shift)))))
    (list 'fixnum->char
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  shl eax, " (- char_shift fixnum_shift))
              (print "  or  eax, " char_tag))))
    (list 'not
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  cmp eax, " (immediate-rep #f))
              (print "  je true")
              (print "  mov eax, " (immediate-rep #f))
              (print "  jmp end")
              (print "true:")
              (print "  mov eax, " (immediate-rep #t))
              (print "end:"))))
    )
  )


