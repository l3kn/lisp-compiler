(define (or? expr) (tagged-list? expr 'or))
(define or-arguments cdr)

(define (or->if expr)
  (define (loop expr)
    (if (null? expr)
        #f
        `(if ,(car expr)
             #t
             ,(loop (cdr expr)))))
  (loop (or-arguments expr)))
