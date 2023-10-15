#lang racket

(require "type-dispatch.rkt")

(provide install-scheme-number-package
         make-scheme-number)

(define (install-scheme-number-package)
  (put 'make 'scheme-number make-scheme-number)
  (put 'add '(scheme-number scheme-number) add)
  (put 'sub '(scheme-number scheme-number) sub)
  (put 'mul '(scheme-number scheme-number) mul)
  (put 'div '(scheme-number scheme-number) div)
  (put 'equ? '(scheme-number scheme-number) equ?)
  (put '=zero? '(scheme-number) =zero?)
  'done)

(define (tag x) (attach-tag 'scheme-number x))

(define (make-scheme-number x) (tag x))

(define (add x y) (tag (+ x y)))
(define (sub x y) (tag (- x y)))
(define (mul x y) (tag (* x y)))
(define (div x y) (tag (/ x y)))
(define (equ? x y) (tag (= x y)))
(define (=zero? x) (= x 0))
