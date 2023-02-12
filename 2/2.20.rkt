#lang racket

(define (same-parity x . xs)
  (if (even? x)
      (cons x (filter even? xs))
      (cons x (filter odd? xs))))
