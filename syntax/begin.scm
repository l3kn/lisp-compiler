(define (begin? expr) (tagged-list? expr 'begin))
(define begin-expressions cdr)

(define (emit-begin stack-index env expr tail)
  (emit-comment "begin")
  (for-each
    (lambda (expr)
      (emit-expr stack-index env expr #f))
    (begin-expressions expr))
  (emit-comment "/begin"))

