(include "primitives.scm")
(include "syntax/and.scm")
(include "syntax/or.scm")

(define wordsize 4)

(define bool_false #b00101111)
(define bool_true  #b01101111)
(define empty_list #b00111111)

(define char_shift 8)
(define char_mask #b11111111)
(define char_tag #b00001111)

(define fixnum_shift 2)
(define fixnum_mask #b11)
(define fixnum_tag #b00)

(define fixnum_bits (- (* wordsize 8) fixnum_shift))
(define fixnum_lower (- (expt 2 (- fixnum_bits 1))))
(define fixnum_upper (sub1 (expt 2 (- fixnum_bits 1))))

(define (tagged-list? expr tag)
  (and (pair? expr)
       (eq? (car expr) tag)))

(define (fixnum? x)
  (and (integer? x)
       (<= fixnum_lower x fixnum_upper)))

(define (immediate? x)
  (or (fixnum? x)
      (boolean? x)
      (char? x)
      (null? x)))

(define (immediate-rep x)
  (cond
    ((fixnum? x) 
     (fxshl x fixnum_shift))
    ((boolean? x)
     (if x bool_true bool_false))
    ((null? x) empty_list)
    ((char? x)
     (+ (fxshl (char->integer x) 8)
        char_tag))
    (else (error "Invalid expression: " x))))

(define (if? expr) (tagged-list? expr 'if))
(define if-test cadr)
(define if-consequent caddr)
(define if-alternative cadddr)

(define (emit-if expr)
  (let ((alt-label (unique-label "alternative"))
        (end-label (unique-label "end")))
    (if (predicate? (if-test expr))
      ((cadr (assoc (car (if-test expr)) raw-predicates)) (cdr (if-test expr)))
      (begin
        (emit-expr (if-test expr))
        (print "  cmp eax, " (immediate-rep #t))))
    (print "  jne " alt-label)
    (emit-expr (if-consequent expr))
    (print "  jmp " end-label)
    (print alt-label ":")
    (emit-expr (if-alternative expr))
    (print end-label ":")))

(define (emit output)
  (print output))

(define (emit-expr expr)
  (cond
    ((immediate? expr)
     (print "  mov eax, " (immediate-rep expr)))
    ((if? expr)
     (emit-if expr))
    ((and? expr) (emit-expr (and->if expr)))
    ((or? expr) (emit-expr (or->if expr)))
    ((primitive? expr)
       ((cadr (assoc (car expr) primitives)) (cdr expr)))
    ((predicate? expr)
       (let ((raw-predicate (assoc (car expr) raw-predicates))
             (true-label (unique-label "true"))
             (end-label (unique-label "end")))
         ((cadr raw-predicate) (cdr expr))
         (print "  je " true-label)
         (print "  mov eax, " (immediate-rep #f))
         (print "  jmp " end-label)
         (print true-label ":")
         (print "  mov eax, " (immediate-rep #t))
         (print end-label ":")))
    (else
      (error "Unknown expression: " expr))))

(define (emit-program expr)
  (emit "  .intel_syntax noprefix")
  (emit "  .text")
  (emit "  .globl scheme_entry")
  (emit "  .type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit-expr expr)
  (emit "  ret")
  (emit "  .size scheme_entry, .-scheme_entry"))

; (emit-program '(fxzero? 1))
; (emit-program '(boolean? #\A))
; (emit-program '(if #t (if (fxzero? 0) #\a #\b) #\c))
; (emit-program '(char->fixnum #\A))
; (emit-program '(fixnum->char 65))
; (emit-program '(not #t))
; (emit-program '(fxlognot -11))
; (emit-program '(or #f #f))
(emit-program '(if (fxzero? 0) #\y #\n))
