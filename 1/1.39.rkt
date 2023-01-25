#lang racket

(define (tan-cf x k)
  (define (d i) (- (* 2 i) 1))
  (define (tan-cf-helper i)
    (cond ((= i 1) (/ x (- (d i) (tan-cf-helper (+ i 1)))))
          ((= i k) (/ (square x) (d i)))
          ((< i k) (/ (square x) (- (d i) (tan-cf-helper (+ i 1)))))
          ((> i k) 0)))
  (tan-cf-helper 1))

(define (square x) (* x x))
