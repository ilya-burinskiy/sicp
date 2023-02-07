#lang racket

(define (last-pair lst)
  (cond ((= (cdr lst) null) (car lst))
        (else (last-pair (cdr lst)))))
