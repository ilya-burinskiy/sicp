#lang racket

(provide last-pair reverse-list)

(define (last-pair lst)
  (cond ((= (cdr lst) null) (car lst))
        (else (last-pair (cdr lst)))))

(define (reverse-list lst)
  (define (reverse-list-helper lst res)
    (if (null? lst)
        res
        (reverse-list-helper (cdr lst) (cons (car lst) res))))
  (reverse-list-helper lst null))
