#lang racket

(require "../type-dispatch.rkt")

(provide make-scheme-number add sub mul div equ? =zero?)

(define (tag x) (attach-tag 'scheme-number x))
(define (make-scheme-number x) (tag x))

(define (add x y) (tag (+ x y)))
(define (sub x y) (tag (- x y)))
(define (mul x y) (tag (* x y)))
(define (div x y) (tag (/ x y)))
(define (equ? x y) (= x y))
(define (=zero? x) (= x 0))
