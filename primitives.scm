(include "label.scm")

(define raw-predicates '())

(define (register-raw-predicate name body)
  (let ((old raw-predicates))
    (set! raw-predicates
          (cons (list name body) old))))

(define primitives '())

(define (register-primitive name body)
  (let ((old primitives))
    (set! primitives
          (cons (list name body) old))))

(register-raw-predicate 'boolean?
      (lambda (stack-index env args)
        (let ((arg (car args)))
          (emit-expr stack-index env arg)
          (print "  and rax, " bool_mask)
          (print "  cmp rax, " bool_tag))))
(register-raw-predicate 'null?
      (lambda (stack-index env args)
        (let ((arg (car args)))
          (emit-expr stack-index env arg)
          (print "  cmp rax, " (immediate-rep '())))))
(register-raw-predicate 'not
      (lambda (stack-index env args)
        (let ((arg (car args)))
          (emit-expr stack-index env arg)
          (print "  cmp rax, " (immediate-rep #f)))))

(define (predicate? expr)
  (and (pair? expr)
       (assoc (car expr) raw-predicates)))

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
