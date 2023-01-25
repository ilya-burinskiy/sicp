#lang racket

(define (double x) (* 2 x))

(define (half x) (/ x 2)) ; x must be even

(define (even? n)
  (= (remainder n 2) 0))

(define (mult a b)
  (define (mult-helper a b res)
    (cond ((= b 0) res)
          ((even? b) (mult-helper (double a) (half b) res))
          (else (mult-helper a (- b 1) (+ res a)))))
  (mult-helper a b 0))
