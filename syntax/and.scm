(define (and? expr) (tagged-list? expr 'and))
(define and-arguments cdr)

(define (and->if expr)
  (define (loop expr)
    (if (null? expr)
        #t
        `(if ,(car expr)
             ,(loop (cdr expr))
             #f)))
  (loop (and-arguments expr)))
