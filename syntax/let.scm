(define (let? expr) (tagged-list? expr 'let))
(define let-bindings cadr)
(define let-body caddr)

(define let-binding-value cadr)
(define let-binding-variable car)

(define (emit-let stack-index env expr tail)
  (define (process-let bindings stack-index new-env)
    (cond
      ((null? bindings)
       (emit-expr stack-index new-env (let-body expr) tail))
      (else
        (let ((b (car bindings)))
          (emit-comment "Binding")
          (emit-stack-save_ stack-index env (let-binding-value b) #f)
          (emit-comment "/Binding")
          (process-let
            (cdr bindings)
            (next-stack-index stack-index)
            (extend-env (let-binding-variable b) stack-index new-env))))))
  (process-let (let-bindings expr) stack-index env))

(define (let*? expr) (tagged-list? expr 'let*))
(define (emit-let* stack-index env expr tail)
  (define (process-let bindings stack-index new-env)
    (cond
      ((null? bindings)
       (emit-expr stack-index new-env (let-body expr) tail))
      (else
        (let ((b (car bindings)))
          (emit-stack-save_ stack-index new-env (let-binding-value b) #f)
          (process-let
            (cdr bindings)
            (next-stack-index stack-index)
            (extend-env (let-binding-variable b) stack-index new-env))))))
  (process-let (let-bindings expr) stack-index env))
