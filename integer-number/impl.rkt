#lang racket

(require "../type-dispatch.rkt")

(provide make-integer-number add sub mul div equ? =zero?)

(define (tag x) (attach-tag 'integer-number x))
(define (make-integer-number x)
  (if (integer? x)
      (make-integer-number x)
      (error "Invalid integer" x)))

(define (add x y) (tag (+ x y)))
(define (sub x y) (tag (- x y)))
(define (mul x y) (* x y))
(define (div x y) (/ x y))
(define (equ? x y) (= x y))
(define (=zero? x) (= x 0))
