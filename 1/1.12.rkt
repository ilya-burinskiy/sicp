#lang racket

(define (pascal m n)
  (cond ((or (= m 0) (> m n)) 0)
        ((and (= m 1) (= n 1)) 1)
        (else (+ (pascal (- m 1) (- n 1))
                 (pascal m (- n 1))))))
