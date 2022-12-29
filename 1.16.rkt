#lang racket

(define (fast-expt-iter b n)
  (define (fast-expt-iter-helper b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter-helper (* b b) (/ n 2) a))
          (else (fast-expt-iter-helper b (- n 1) (* a b)))))
  (fast-expt-iter-helper b n 1))

(define (even? n)
  (= (remainder n 2) 0))
