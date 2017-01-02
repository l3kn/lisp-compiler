(include "label.scm")

(define raw-predicates
  (list
    (list 'fxzero?
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  cmp eax, " (immediate-rep 0)))))
    (list 'fixnum?
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  and eax, " fixnum_mask)
              (print "  cmp eax, " fixnum_tag))))
    (list 'char?
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  and eax, " char_mask)
              (print "  cmp eax, " char_tag))))
    (list 'boolean?
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  and eax, " bool_mask)
              (print "  cmp eax, " bool_tag))))
    (list 'null?
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  cmp eax, " (immediate-rep '())))))
    (list 'not
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  cmp eax, " (immediate-rep #f)))))
  )
)

(define (predicate? expr)
  (and (pair? expr)
       (assoc (car expr) raw-predicates)))

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
    (list 'fxlognot
          (lambda (args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  shr eax, " fixnum_shift)
              (print "  not eax")
              (print "  shl eax, " fixnum_shift))))
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
            (let ((arg (car args))
                  (l_true (unique-label "true"))
                  (l_end (unique-label "end")))
              (print "  mov eax, " (immediate-rep arg))
              (print "  cmp eax, " (immediate-rep #f))
              (print "  je " l_true)
              (print "  mov eax, " (immediate-rep #f))
              (print "  jmp " l_end)
              (print l_true ":")
              (print "  mov eax, " (immediate-rep #t))
              (print l_end ":"))))
    )
  )

(define (primitive? expr)
  (and (pair? expr)
       (assoc (car expr) primitives)))
