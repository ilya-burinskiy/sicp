#lang racket

(define (sqrt x)
  (define init-guess 1.0)
  (define (sqrt-iter prev-guess current-guess x)
    (define (good-enough? prev-guess current-guess x)
      (< (abs (- (square current-guess) x)) 0.001))
    (define (improve guess x)
      (average guess (/ x guess)))
    (if (good-enough? prev-guess current-guess x)
        current-guess
        (sqrt-iter current-guess
                   (improve current-guess x)
                   x)))
  (sqrt-iter 0.0 init-guess x))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))
