#lang racket

(define (sqrt x)
  (define init-guess 1.0)
  (define (sqrt-iter current-guess x)
    (define (good-enough? current-guess x)
      (< (abs (- (square current-guess) x)) 0.001))
    (define (improve guess x)
      (average guess (/ x guess)))
    (if (good-enough? current-guess x)
        current-guess
        (sqrt-iter (improve current-guess x)
                   x)))
  (sqrt-iter init-guess x))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))
