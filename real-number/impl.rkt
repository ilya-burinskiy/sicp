#lang racket

(require "../type-dispatch.rkt")

(provide make-real-number add sub mul div equ? =zero?)

(define (tag x) (attach-tag 'real-number x))
(define (make-real-number x) (tag x))

(define (add x y) (tag (+ x y)))
(define (sub x y) (tag (- x y)))
(define (mul x y) (tag (* x y)))
(define (div x y) (tag (/ x y)))
(define (equ? x y) (= x y))
; NOTE: maybe use abs(x - eps) <= 0.0
(define (=zero? x) (= x 0.0))
