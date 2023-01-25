#lang racket

(define (f n)
  (if (< n 3)
      3
      (+ (f (- n 1))
         (f (- n 2))
         (f (- n 3)))))

(define (f-iter n)
  (define (f-iter-helper a b c n)
    (if (< n 3)
        c
        (f-iter-helper b c (+ a b c) (- n 1))))
  (f-iter-helper 3 3 3 n))
