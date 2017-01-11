(include "label.scm")

(define raw-predicates
  (list
    (list 'fxzero?
          (lambda (stack-index env args)
            (let ((arg (car args)))
              (emit-expr stack-index env arg)
              (print "  cmp rax, " (immediate-rep 0)))))
    (list 'fixnum?
          (lambda (stack-index env args)
            (let ((arg (car args)))
              (emit-expr stack-index env arg)
              (print "  and rax, " fixnum_mask)
              (print "  cmp rax, " fixnum_tag))))
    (list 'char?
          (lambda (stack-index env args)
            (let ((arg (car args)))
              (emit-expr stack-index env arg)
              (print "  and rax, " char_mask)
              (print "  cmp rax, " char_tag))))
    (list 'boolean?
          (lambda (stack-index env args)
            (let ((arg (car args)))
              (emit-expr stack-index env arg)
              (print "  and rax, " bool_mask)
              (print "  cmp rax, " bool_tag))))
    (list 'null?
          (lambda (stack-index env args)
            (let ((arg (car args)))
              (emit-expr stack-index env arg)
              (print "  cmp rax, " (immediate-rep '())))))
    (list 'not
          (lambda (stack-index env args)
            (let ((arg (car args)))
              (emit-expr stack-index env arg)
              (print "  cmp rax, " (immediate-rep #f)))))
  )
)

(define (predicate? expr)
  (and (pair? expr)
       (assoc (car expr) raw-predicates)))

(define primitives
  (list 
    (list 'fxadd1
          (lambda (stack-index env args)
            (let ((arg (car args)))
              (emit-expr stack-index env arg)
              (print "  add rax, " (immediate-rep 1)))))
    (list 'fxsub1
          (lambda (stack-index env args)
            (let ((arg (car args)))
              (emit-expr stack-index env arg)
              (print "  sub rax, " (immediate-rep 1)))))
    (list 'fxlognot
          (lambda (stack-index env args)
            (let ((arg (car args)))
              (emit-expr stack-index env arg)
              (print "  shr rax, " fixnum_shift)
              (print "  not rax")
              (print "  shl rax, " fixnum_shift))))
    (list 'char->fixnum
          (lambda (stack-index env args)
            (let ((arg (car args)))
              (emit-expr stack-index env arg)
              (print "  shr rax, " (- char_shift fixnum_shift)))))
    (list 'fixnum->char
          (lambda (stack-index env args)
            (let ((arg (car args)))
              (emit-expr stack-index env arg)
              (print "  shl rax, " (- char_shift fixnum_shift))
              (print "  or  rax, " char_tag))))
    (list 'not
          (lambda (stack-index env args)
            (let ((arg (car args))
                  (l_true (unique-label "true"))
                  (l_end (unique-label "end")))
              (emit-expr stack-index env arg)
              (print "  cmp rax, " (immediate-rep #f))
              (print "  je " l_true)
              (emit-immediate #f)
              (print "  jmp " l_end)
              (print l_true ":")
              (emit-immediate #t)
              (print l_end ":"))))
    (list 'fx+
          (lambda (stack-index env args)
            (emit-binop stack-index env (car args) (cadr args) "add")))
    (list 'fx-
          (lambda (stack-index env args)
            ; Swap the arguments around,
            ; we want to evaluate the cadr first,
            ; put it on the stack,
            ; evaluate the car to rax
            ; and then sub the value of the cadr from it
            (emit-binop stack-index env (cadr args) (car args) "sub")))
    (list 'fxlogand
          (lambda (stack-index env args)
            (emit-binop stack-index env (car args) (cadr args) "and")))
    (list 'fxlogor
          (lambda (stack-index env args)
            (emit-binop stack-index env (car args) (cadr args) "or")))
    (list 'fx*
          (lambda (stack-index env args)
            (let ((arg1 (car args))
                  (arg2 (cadr args)))
              (emit-expr stack-index env arg2)
              (print "  shr rax, " fixnum_shift)
              (print "  mov [rsp - " stack-index "], rax")
              (emit-expr (next-stack-index stack-index) env arg1)
              (print "  shr rax, " fixnum_shift)
              (print "  imul rax, [rsp - " stack-index "]")
              (print "  shl rax, " fixnum_shift))))
    (list 'fx=?
          (lambda (stack-index env args)
            (emit-comparison stack-index env (car args) (cadr args))
            (emit-test stack-index env "je")))
    (list 'fx<?
          (lambda (stack-index env args)
            (emit-comparison stack-index env (car args) (cadr args))
            (emit-test stack-index env "jl")))
    (list 'fx<=?
          (lambda (stack-index env args)
            (emit-comparison stack-index env (car args) (cadr args))
            (emit-test stack-index env "jlt")))
    (list 'fx>?
          (lambda (stack-index env args)
            (emit-comparison stack-index env (car args) (cadr args))
            (emit-test stack-index env "jg")))
    (list 'fx>=?
          (lambda (stack-index env args)
            (emit-comparison stack-index env (car args) (cadr args))
            (emit-test stack-index env "jgt")))
    (list 'char=?
          (lambda (stack-index env args)
            (emit-comparison stack-index env (car args) (cadr args))
            (emit-test stack-index env "je")))
    (list 'char<?
          (lambda (stack-index env args)
            (emit-comparison stack-index env (car args) (cadr args))
            (emit-test stack-index env "jl")))
    (list 'char<=?
          (lambda (stack-index env args)
            (emit-comparison stack-index env (car args) (cadr args))
            (emit-test stack-index env "jlt")))
    (list 'char>?
          (lambda (stack-index env args)
            (emit-comparison stack-index env (car args) (cadr args))
            (emit-test stack-index env "jg")))
    (list 'char>=?
          (lambda (stack-index env args)
            (emit-comparison stack-index env (car args) (cadr args))
            (emit-test stack-index env "jgt")))
    (list 'car
          (lambda (stack-index env args)
            (emit-expr stack-index env (car args))
            ; Remove the tag of the pair value
            ; to get the address
            (print "  and al, " #b11111000)
            (print "  mov rax, [rax]")))
    (list 'cdr
          (lambda (stack-index env args)
            (emit-expr stack-index env (car args))
            ; Remove the tag of the pair value
            ; to get the address
            (print "  and al, " #b11111000)
            (print "  mov rax, [rax + 8]")))
    )
  )

(define (emit-binop stack-index env arg1 arg2 binop)
  (emit-expr stack-index env arg2)
  (if (immediate? arg1)
    (print "  " binop " rax, " (immediate-rep arg1))
    (begin
      (print "  mov [rsp - " stack-index "], rax")
      (emit-expr (next-stack-index stack-index) env arg1)
      (print "  " binop " rax, [rsp - " stack-index "]"))))

(define (emit-test stack-index env jump)
  (let ((true-label (unique-label "true"))
        (end-label (unique-label "end")))
    (print "  " jump " " true-label)
    (print "  mov rax, " (immediate-rep #f))
    (print "  jmp " end-label)
    (print true-label ":")
    (print "  mov rax, " (immediate-rep #t))
    (print end-label ":")))

(define (emit-comparison stack-index env arg1 arg2)
  (emit-expr stack-index env arg2)
  (if (immediate? arg1)
    (print "  cmp rax, " (immediate-rep arg1))
    (begin
      (print "  mov [rsp - " stack-index "], rax")
      (emit-expr (next-stack-index stack-index) arg1)
      (print "  cmp rax, [rsp - " stack-index "]"))))

(define (primitive? expr)
  (and (pair? expr)
       (assoc (car expr) primitives)))
