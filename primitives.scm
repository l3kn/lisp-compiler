(include "label.scm")

(define raw-predicates
  (list
    (list 'fxzero?
          (lambda (stack-index args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  cmp eax, " (immediate-rep 0)))))
    (list 'fixnum?
          (lambda (stack-index args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  and eax, " fixnum_mask)
              (print "  cmp eax, " fixnum_tag))))
    (list 'char?
          (lambda (stack-index args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  and eax, " char_mask)
              (print "  cmp eax, " char_tag))))
    (list 'boolean?
          (lambda (stack-index args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  and eax, " bool_mask)
              (print "  cmp eax, " bool_tag))))
    (list 'null?
          (lambda (stack-index args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  cmp eax, " (immediate-rep '())))))
    (list 'not
          (lambda (stack-index args)
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
          (lambda (stack-index args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  add eax, " (immediate-rep 1)))))
    (list 'fxsub1
          (lambda (stack-index args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  sub eax, " (immediate-rep 1)))))
    (list 'fxlognot
          (lambda (stack-index args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  shr eax, " fixnum_shift)
              (print "  not eax")
              (print "  shl eax, " fixnum_shift))))
    (list 'char->fixnum
          (lambda (stack-index args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  shr eax, " (- char_shift fixnum_shift)))))
    (list 'fixnum->char
          (lambda (stack-index args)
            (let ((arg (car args)))
              (print "  mov eax, " (immediate-rep arg))
              (print "  shl eax, " (- char_shift fixnum_shift))
              (print "  or  eax, " char_tag))))
    (list 'not
          (lambda (stack-index args)
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
    (list 'fx+
          (lambda (stack-index args)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (emit-expr stack-index arg1)
              (print "  mov DWORD PTR [rsp - " stack-index "], eax")
              (emit-expr (+ stack-index wordsize) arg2)
              (print "  add eax, DWORD PTR [rsp - " stack-index "]"))))
    (list 'fx-
          (lambda (stack-index args)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (emit-expr stack-index arg2)
              (print "  mov DWORD PTR [rsp - " stack-index "], eax")
              (emit-expr (+ stack-index wordsize) arg1)
              (print "  sub eax, DWORD PTR [rsp - " stack-index "]"))))
    (list 'fxlogand
          (lambda (stack-index args)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (emit-expr stack-index arg2)
              (print "  mov DWORD PTR [rsp - " stack-index "], eax")
              (emit-expr (+ stack-index wordsize) arg1)
              (print "  and eax, DWORD PTR [rsp - " stack-index "]"))))
    (list 'fxlogor
          (lambda (stack-index args)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (emit-expr stack-index arg2)
              (print "  mov DWORD PTR [rsp - " stack-index "], eax")
              (emit-expr (+ stack-index wordsize) arg1)
              (print "  or eax, DWORD PTR [rsp - " stack-index "]"))))
    (list 'fx=?
          (lambda (stack-index args)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (emit-expr stack-index arg2)
              (print "  mov DWORD PTR [rsp - " stack-index "], eax")
              (emit-expr (+ stack-index wordsize) arg1)
              (print "  cmp eax, DWORD PTR [rsp - " stack-index "]")
              (emit-test "je"))))
    (list 'fx<?
          (lambda (stack-index args)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (emit-expr stack-index arg2)
              (print "  mov DWORD PTR [rsp - " stack-index "], eax")
              (emit-expr (+ stack-index wordsize) arg1)
              (print "  cmp eax, DWORD PTR [rsp - " stack-index "]")
              (emit-test "jl"))))
    (list 'fx<=?
          (lambda (stack-index args)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (emit-expr stack-index arg2)
              (print "  mov DWORD PTR [rsp - " stack-index "], eax")
              (emit-expr (+ stack-index wordsize) arg1)
              (print "  cmp eax, DWORD PTR [rsp - " stack-index "]")
              (emit-test "jlt"))))
    (list 'fx>?
          (lambda (stack-index args)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (emit-expr stack-index arg2)
              (print "  mov DWORD PTR [rsp - " stack-index "], eax")
              (emit-expr (+ stack-index wordsize) arg1)
              (print "  cmp eax, DWORD PTR [rsp - " stack-index "]")
              (emit-test "jg"))))
    (list 'fx>=?
          (lambda (stack-index args)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (emit-expr stack-index arg2)
              (print "  mov DWORD PTR [rsp - " stack-index "], eax")
              (emit-expr (+ stack-index wordsize) arg1)
              (print "  cmp eax, DWORD PTR [rsp - " stack-index "]")
              (emit-test "jgt"))))
    (list 'char=?
          (lambda (stack-index args)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (emit-expr stack-index arg2)
              (print "  mov DWORD PTR [rsp - " stack-index "], eax")
              (emit-expr (+ stack-index wordsize) arg1)
              (print "  cmp eax, DWORD PTR [rsp - " stack-index "]")
              (emit-test "je"))))
    (list 'char<?
          (lambda (stack-index args)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (emit-expr stack-index arg2)
              (print "  mov DWORD PTR [rsp - " stack-index "], eax")
              (emit-expr (+ stack-index wordsize) arg1)
              (print "  cmp eax, DWORD PTR [rsp - " stack-index "]")
              (emit-test "jl"))))
    (list 'char<=?
          (lambda (stack-index args)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (emit-expr stack-index arg2)
              (print "  mov DWORD PTR [rsp - " stack-index "], eax")
              (emit-expr (+ stack-index wordsize) arg1)
              (print "  cmp eax, DWORD PTR [rsp - " stack-index "]")
              (emit-test "jlt"))))
    (list 'char>?
          (lambda (stack-index args)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (emit-expr stack-index arg2)
              (print "  mov DWORD PTR [rsp - " stack-index "], eax")
              (emit-expr (+ stack-index wordsize) arg1)
              (print "  cmp eax, DWORD PTR [rsp - " stack-index "]")
              (emit-test "jg"))))
    (list 'char>=?
          (lambda (stack-index args)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (emit-expr stack-index arg2)
              (print "  mov DWORD PTR [rsp - " stack-index "], eax")
              (emit-expr (+ stack-index wordsize) arg1)
              (print "  cmp eax, DWORD PTR [rsp - " stack-index "]")
              (emit-test "jgt"))))
    )
  )

(define (emit-test jump)
  (let ((true-label (unique-label "true"))
        (end-label (unique-label "end")))
    (print "  " jump " " true-label)
    (print "  mov eax, " (immediate-rep #f))
    (print "  jmp " end-label)
    (print true-label ":")
    (print "  mov eax, " (immediate-rep #t))
    (print end-label ":")))

(define (primitive? expr)
  (and (pair? expr)
       (assoc (car expr) primitives)))
