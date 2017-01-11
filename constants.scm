(define wordsize 8)

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

(define pair_mask #b111)
(define pair_tag #b001)
