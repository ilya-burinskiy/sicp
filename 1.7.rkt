#lang racket

(define (sqrt x)
  (define init-guess 1.0)
  (define (sqrt-iter prev-guess current-guess)
    (define (good-enough? prev-guess current-guess)
      (< (abs (- (square current-guess) x)) 0.001))
    (define (improve guess)
      (average guess (/ x guess)))
    (if (good-enough? prev-guess current-guess)
        current-guess
        (sqrt-iter current-guess
                   (improve current-guess))))
  (sqrt-iter 0.0 init-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))
