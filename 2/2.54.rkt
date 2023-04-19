#lang racket

(define (equal?_ a b)
 (or (and
      (not (pair? a))
      (not (pair? b))
      (eq? a b))
     (and
      (pair? a)
      (pair? b)
      (equal?_ (car a) (car b))
      (equal?_ (cdr a) (cdr b)))))
