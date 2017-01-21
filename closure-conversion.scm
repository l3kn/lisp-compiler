(include "compile.scm")

(define (union s1 s2)
  (cond
    ((null? s2) s1)
    ((member (car s2) s1) s1)
    (else (union (cons (car s2) s1)
                 (cdr s2)))))

(define (lambda? expr) (tagged-list? 'lambda expr))
(define lambda-vars cadr)
(define lambda-body cddr)

(define num-environments 0)
(define environments '())

(define (allocate-environment fields)
  (let ((id num-environments))
    (set! num-environments (add1 num-environments))
    (set! environments
      (cons (cons id fields) environments))
    id))

(define (get-environment id)
  (cdr (assoc id environments)))

(define (closure-convert expr)
  (cond
    ((immediate? expr) expr)
    ((string? expr) expr)
    ((if? expr)
     `(if ,(closure-convert (if-test expr))
          ,(closure-convert (if-consequent expr))
          ,(closure-convert (if-alternative expr))))
    ((begin? expr)
     (cons 'begin
           (map closure-convert (begin-expressions expr))))
    ((and? expr) (closure-convert (and->if expr)))
    ((or? expr) (closure-convert (or->if expr)))
    ; TODO:
    ; Let is pretty special bc. it is able to create own var bindings
    ((let? expr) `(let ,(map closure-convert (let-bindings expr))
                       ,(closure-convert (let-body expr))))
    ((let*? expr) `(let* ,(map closure-convert (let-bindings expr))
                         ,(closure-convert (let-body expr))))
    ((list? expr)
     (map closure-convert expr))
    ((variable? expr)
     (emit-variable-ref env expr tail))
    (else
      (error "Unknown expression: " expr))))

(define (closure-conversion expr)
  (let* ((bound-vars (lambda-vars expr))
         (body (lambda-body expr))
         (new-body (remove-free-variables body '())))
  `(closure (lambda ,(cons 'env (lambda-vars expr))
                    ,(cons 'begin (lambda-body expr))))))

(define (free-variables expr)
  (cond
    ((immediate? expr) '())
    ((string? expr) '())
    ((if? expr)
     (union (free-variables (if-test expr))
            (union (free-variables (if-consequent expr))
                   (free-variables (if-alternative expr)))))
    ((begin? expr)
     (free-variables-list (begin-expressions expr)))
    ((and? expr) (free-variables (and->if expr)))
    ((or? expr) (free-variables (or->if expr)))
    ; TODO:
    ; Let is pretty special bc. it is able to create own var bindings
    ; ((let? expr) `(let ,(map closure-convert (let-bindings expr))
    ;                    ,(closure-convert (let-body expr))))
    ; ((let*? expr) `(let* ,(map closure-convert (let-bindings expr))
    ;                      ,(closure-convert (let-body expr))))
    ((list? expr)
     (if (list? (car expr))
         (union (free-variables (car expr))
                (free-variables-list (cdr expr)))
         (free-variables-list (cdr expr))))
    ((variable? expr)
     (list expr))
    (else
      (error "Unknown expression: " expr))))

(define (free-variables-list lst)
  (if (null? lst)
      '()
      (union (free-variables (car lst))
             (free-variables-list (cdr lst)))))


; (display (closure-conversion '(lambda (x y) (+ x y z))))

(display (free-variables
             '(+ x y z
                (if (= q v) a b))))
