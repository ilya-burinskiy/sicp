#lang racket

(define (smooth f)
  (define dx 1.e-5)
  (lambda (x)
    (let ((f1 (f (- x dx))
          (f2 (f x))
          (f3 (f (+ x dx)))))
      (/ (+ f1 f2 f3) 3))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      (compose f identity)
      (compose f (repeated f (- n 1)))))

; was wrong
(define (n-folded-smooth f n) (repeated smooth n) f)
