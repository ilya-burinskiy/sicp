#lang racket

(define (cube-root x)
  (define init-guess 1.0)
  (define (cube-root-iter prev-guess current-guess)
    (define eps 0.000001)
    (define (good-enough? prev-guess current-guess)
      (define guess-diff (abs (- prev-guess current-guess)))
      (< guess-diff eps))
    (define (improve guess)
      (/ (+ (/ x (* guess guess)) (* 2 guess))
         3))
    (if (good-enough? prev-guess current-guess)
        current-guess
        (cube-root-iter current-guess
                        (improve current-guess))))
  (cube-root-iter 0.0 init-guess))
