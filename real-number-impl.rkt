#lang racket

(require "type-dispatch.rkt")

(provide install-real-number-package
         make-real-number)

(define (install-real-number-package)
  (put 'make 'real-number make-real-number)
  (put 'add '(real-number real-number) add)
  (put 'sub '(real-number real-number) sub)
  (put 'mul '(real-number real-number) mul)
  (put 'div '(real-number real-number) div)
  (put 'equ? '(real-number real-number) equ?)
  (put '=zero? '(real-number) =zero?)
  'done)


(define (tag x) (attach-tag 'real-number x))
(define (make-real-number x) (tag x))

(define (add x y) (tag (+ x y)))
(define (sub x y) (tag (- x y)))
(define (mul x y) (tag (* x y)))
(define (div x y) (tag (/ x y)))
(define (equ? x y) (= x y))
; NOTE: maybe use abs(x - eps) <= 0.0
(define (=zero? x) (= x 0.0))
