#lang racket

(define (for-each f xs)
  (when (not (null? xs))
    (let ((head (car xs))
          (tail (cdr xs)))
      (f head)
      (for-each f tail))))
