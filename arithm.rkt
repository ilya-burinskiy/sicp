#lang racket

(require "integer-number-impl.rkt"
         "rational-number-impl.rkt"
         "real-number/init.rkt"
         "complex-number/impl.rkt"
         "type-dispatch.rkt")

(provide (all-from-out "complex-number/impl.rkt")
         (all-from-out "integer-number-impl.rkt")
         (all-from-out "rational-number-impl.rkt")
         add sub mul div equ? =zero?)

(install-integer-number-package)
(install-rational-package)
(install-real-number-package)
(install-complex-package)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
