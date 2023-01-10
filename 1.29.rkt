#lang racket

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (define y (f (+ a (* k h))))
    (if (= (remainder k 2) 0)
        (* 2 y)
        (* 4 y)))
  (define y0 (f a))
  (define yn (f (+ a (* h n))))
  (define (next i) (+ i 1))
  (* (/ h 3.0)
     (+ y0 (sum term 1 next (- n 1)) yn)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
