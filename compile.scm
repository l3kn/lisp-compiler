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

(define (fixnum? x)
  (and (integer? x)
       (<= fixnum_lower x fixnum_upper)))


(define (immediate? x)
  (or (fixnum? x)
      (boolean? x)))

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

(define (emit output)
  (print output))

(define (compile x)
  (emit "  .intel_syntax noprefix")
  (emit "  .text")
  (emit "  .globl scheme_entry")
  (emit "  .type scheme_entry, @function")
  (emit "scheme_entry:")
  (print "  mov eax, " (immediate-rep x))
  (emit "  ret")
  (emit "  .size scheme_entry, .-scheme_entry"))

(compile #\b)
