#lang racket

(provide sum-squares-of-two-largest)

(define (square x)
  (* x x))

(define (sum-squares-of-two-largest x y z)
  (cond ((< x y)
         (cond ((< y z) (+ (square y) (square z)))
               ((= y z) (+ (square x) (square y)))
               ((< z y) (+ (square y) (square (max x z))))))
        ((< y x)
         (cond ((< x z) (+ (square x) (square z)))
               ((= x z) (+ (square x) (square y)))
               ((< z x) (+ (square x) (square (max y z))))))
        ((= x y) (+ (square y) (square z)))))
