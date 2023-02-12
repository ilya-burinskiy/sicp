#lang racket

(define (square-list-1 items)
  (if (null? items)
      null
      (let ((head (car items))
            (tail (cdr items))
            (square (lambda (x) (* x x))))
       (cons (square head) (square-list-1 tail)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))
