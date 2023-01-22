#lang racket

(define (double f)
  (lambda (x) (f (f x))))

; (double (double double)) inc $ 5

; (double double) = (\x -> (\y -> (double x) ((double x) y)))
; (\x -> double (double x))
; (\x -> (\y -> (double x) ((double x) y)))

; (double f) inc $ 5
; f = (\x -> (\y -> (double x) ((double x) y)))
; (\x -> f (f x)) inc $ 5
; f (f inc) $ 5

; (f inc) = \y -> (\x -> x + 2) ((\x -> x + 2) y)
; (\x -> (\y -> (double x) ((double x) y))) inc
; \y -> (\x -> x + 2) ((\x -> x + 2) y)

; f (\y -> (\x -> x + 2) ((\x -> x + 2) y)) = (\y -> (double (\y -> (\x -> x + 2) ((\x -> x + 2) y))) ((double (\y -> (\x -> x + 2) ((\x -> x + 2) y))) y))
; (\x -> (\y -> (double x) ((double x) y))) (\y -> (\x -> x + 2) ((\x -> x + 2) y))
; (\y -> (double (\y -> (\x -> x + 2) ((\x -> x + 2) y))) ((double (\y -> (\x -> x + 2) ((\x -> x + 2) y))) y))

; (\y -> (double (\y -> (\x -> x + 2) ((\x -> x + 2) y))) ((double (\y -> (\x -> x + 2) ((\x -> x + 2) y))) y)) 5
; (double (\y -> (\x -> x + 2) ((\x -> x + 2) y))) ((double (\y -> (\x -> x + 2) ((\x -> x + 2) y))) 5)
; (double (\y -> (\x -> x + 2) ((\x -> x + 2) y))) ((double (\y -> (\x -> x + 2) ((\x -> x + 2) y))) 5)

; (double (\y -> (\x -> x + 2) ((\x -> x + 2) y))) 5
; (\x -> (\y -> (\x -> x + 2) ((\x -> x + 2) y)) ((\y -> (\x -> x + 2) ((\x -> x + 2) y)) x)) 5
; (\y -> (\x -> x + 2) ((\x -> x + 2) y)) ((\y -> (\x -> x + 2) ((\x -> x + 2) y)) 5)
; (\y -> (\x -> x + 2) ((\x -> x + 2) y)) ((\x -> x + 2) ((\x -> x + 2) 5))
; (\y -> (\x -> x + 2) ((\x -> x + 2) y)) ((\x -> x + 2) 7)
; (\y -> (\x -> x + 2) ((\x -> x + 2) y)) 9
; (\x -> x + 2) ((\x -> x + 2) 9)
; (\x -> x + 2) 11
; 13

; (double (\y -> (\x -> x + 2) ((\x -> x + 2) y))) 13
; (\x -> (\y -> (\x -> x + 2) ((\x -> x + 2) y)) ((\y -> (\x -> x + 2) ((\x -> x + 2) y)) x)) 13
; (\y -> (\x -> x + 2) ((\x -> x + 2) y)) ((\y -> (\x -> x + 2) ((\x -> x + 2) y)) 13)
; (\y -> (\x -> x + 2) ((\x -> x + 2) y)) ((\x -> x + 2) ((\x -> x + 2) 13))
; (\y -> (\x -> x + 2) ((\x -> x + 2) y)) ((\x -> x + 2) 15)
; (\y -> (\x -> x + 2) ((\x -> x + 2) y)) 17
; (\x -> x + 2) ((\x -> x + 2) 17)
; (\x -> x + 2) 19
; 21
