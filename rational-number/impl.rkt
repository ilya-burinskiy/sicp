#lang racket

(require "../type-dispatch.rkt" "../math.rkt")

(provide make-rat
         numer
         denom
         add
         sub
         mul
         div
         equ?
         =zero?)

(define (tag x) (attach-tag 'rational x))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (tag (cons (/ n g) (/ d g)))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul x y)
  (make-rat (* (numer x) (numer y))
            (* (denom y) (denom x))))
(define (div x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equ? x y)
  (and (= (numer x) (numer y))
       (= (denom x) (denom y))))
(define (=zero? x) (= (numer x) 0))
