(define (letrec? expr) (tagged-list? expr 'letrec))

(define letrec-bindings cadr)
(define letrec-body caddr)

(define letrec-binding-variable car)
(define letrec-binding-value cadr)

(define (emit-letrec expr)
  (let* ((bindings (letrec-bindings expr))
         (lvars (map letrec-binding-variable bindings))
         (lambdas (map letrec-binding-value bindings))
         (labels (map (lambda (l)
                        (unique-label (symbol->string l)))
                      lvars))
         (env (make-initial-env lvars labels)))
    (for-each (lambda (expr label)
                (emit-lambda env expr label))
              lambdas labels)
    (emit-scheme-entry (letrec-body expr) env)))

(define lambda-formals cadr)
(define lambda-body caddr)

(define (emit-lambda env expr label)
  (emit-function-header label)
  (let ((formals (lambda-formals expr))
        (body (lambda-body expr))
        (stack-index wordsize))
    (define (loop formals stack-index env)
      (if (null? formals)
          (emit-expr stack-index env body #t)
          (loop (cdr formals)
                (next-stack-index stack-index)
                (extend-env (car formals) stack-index env))))
    (loop formals stack-index env))
    (print "  ret"))

(define (apply? expr) (tagged-list? expr 'apply))

(define call-target cadr)
(define call-args cddr)

(define (emit-apply stack-index env expr tail)
  ; (print "SI: " stack-index)
  (define (emit-arguments stack-index args)
    (if (not (null? args))
      (begin
        (emit-stack-save stack-index env (car args))
        (emit-arguments (next-stack-index stack-index) (cdr args)))))
  (define (move-arguments stack-index offset args)
    (if (not (null? args))
      (begin
        (print "  mov rax, [rsp - " (+ stack-index offset) "]")
        (print "  mov [rsp - " stack-index "], rax")
        (move-arguments (next-stack-index stack-index) offset (cdr args)))))
  (if tail
    (begin
      (print "  # evaluate arguments")
      (emit-arguments (next-stack-index stack-index) (call-args expr))
      (print "  # TCO, move arguments down")
      (let ((offset (* wordsize (add1 (length (call-args expr))))))
        (move-arguments (prev-stack-index (prev-stack-index stack-index)) offset (call-args expr)))
      (emit-jump stack-index (lookup (call-target expr) env) env))
    (begin
      (emit-arguments (next-stack-index stack-index) (call-args expr))
      (emit-adjust-base (prev-stack-index stack-index))
      (emit-call stack-index (lookup (call-target expr) env) env)
      (emit-adjust-base (- (prev-stack-index stack-index))))))

(define (emit-adjust-base offset)
  (if (< 0 offset)
    (print "  sub rsp, " offset)
    (print "  add rsp, " (- offset))))

(define (emit-call stack-index target env)
  (print "  call " target))

(define (emit-jump stack-index target env)
  (print "  jmp " target))

(define (lookup-primitive name)
  (let ((result (assoc name primitives)))
    (if result
      (lambda (stack-index env args)
        ((cadr result) stack-index env args))
      (error "No such primitive: " name))))

(define (lookup-raw-predicate name)
  (let ((result (assoc name raw-predicates)))
    (if result
      (lambda (stack-index env args)
        ((cadr result) stack-index env args))
      (error "No such primitive: " name))))
