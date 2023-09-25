#lang racket

(require "type-dispatch.rkt")

(provide install-integer-number-package
         make-integer-number)

(define (install-integer-number-package)
  (put 'make 'integer-number make-integer-number)
  (put 'add '(integer-number integer-number) add)
  (put 'sub '(integer-number integer-number) sub)
  (put 'mul '(integer-number integer-number) mul)
  (put 'div '(integer-number integer-number) div)
  (put 'equ? '(integer-number integer-number) equ?)
  (put '=zero? '(integer-number) =zero?)
  'done)

(define (tag x) (attach-tag 'integer-number x))
(define (make-integer-number x)
  (if (integer? x)
      (tag x)
      (error "Invalid integer" x)))

(define (add x y) (tag (+ x y)))
(define (sub x y) (tag (- x y)))
(define (mul x y) (* x y))
(define (div x y) (/ x y))
(define (equ? x y) (= x y))
(define (=zero? x) (= x 0))
