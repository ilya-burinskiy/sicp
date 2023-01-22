#lang racket

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      (compose f identity)
      (compose f (repeated f (- n 1)))))
