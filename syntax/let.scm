(define (let? expr) (tagged-list? expr 'let))
(define let-bindings cadr)
(define let-body caddr)

(define let-binding-value cadr)
(define let-binding-variable car)

(define (emit-let stack-index env expr)
  (define (process-let bindings stack-index new-env)
    (cond
      ((null? bindings)
       (emit-expr stack-index new-env (let-body expr)))
      (else
        (let ((b (car bindings)))
          (emit-stack-save stack-index env (let-binding-value b))
          (process-let
            (cdr bindings)
            (next-stack-index stack-index)
            (extend-env (let-binding-variable b) stack-index new-env))))))
  (process-let (let-bindings expr) stack-index env))

(define (emit-tail-let stack-index env expr)
  (define (process-let bindings stack-index new-env)
    (cond
      ((null? bindings)
       (emit-tail-expr stack-index new-env (let-body expr)))
      (else
        (let ((b (car bindings)))
          (emit-stack-save stack-index env (let-binding-value b))
          (process-let
            (cdr bindings)
            (next-stack-index stack-index)
            (extend-env (let-binding-variable b) stack-index new-env))))))
  (process-let (let-bindings expr) stack-index env))

(define (let*? expr) (tagged-list? expr 'let*))
(define (emit-let* stack-index env expr)
  (define (process-let bindings stack-index new-env)
    (cond
      ((null? bindings)
       (emit-expr stack-index new-env (let-body expr)))
      (else
        (let ((b (car bindings)))
          (emit-stack-save stack-index new-env (let-binding-value b))
          (process-let
            (cdr bindings)
            (next-stack-index stack-index)
            (extend-env (let-binding-variable b) stack-index new-env))))))
  (process-let (let-bindings expr) stack-index env))

(define (emit-tail-let* stack-index env expr)
  (define (process-let bindings stack-index new-env)
    (cond
      ((null? bindings)
       (emit-tail-expr stack-index new-env (let-body expr)))
      (else
        (let ((b (car bindings)))
          (emit-stack-save stack-index new-env (let-binding-value b))
          (process-let
            (cdr bindings)
            (next-stack-index stack-index)
            (extend-env (let-binding-variable b) stack-index new-env))))))
  (process-let (let-bindings expr) stack-index env))
