#lang racket

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (+ a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

; zero = \f x -> x
; add1 = \n f x -> f $ n f x
; f $ (n f) x
; f $ n f x
; add1 zero
; add1 (\f x -> x)
; (\n f x -> f $ n f x) (\f x -> x)
; \f x -> f $ (\f x -> x) f x
; \f x -> f $ (\x -> x) x
; \f x -> f $  x
; \f x -> f x // пусть это one
;
; (\n f x -> f $ n f x) (\f x -> f x)
; \f x -> f $ (\f x -> f x) f x
; \f x -> f $ f x // пусть это two
;
; (\f x -> f $ f x) `+` (\f x -> f x) = (\f x -> f $ f $ f x)
;
; (\f x -> f $ f x) (\f x -> f x)
; \x -> (\f x -> f x) $ (\f x -> f x) x
; \z -> (\f x -> f x) $ (\f x -> f x) z
; \z -> (\f x -> f x) $ \x -> z x
; \z -> (\f x -> f x) $ \y -> z y
; \z -> (\x -> (\y -> z y) x)
; \z x -> (\y -> z y) x
; \z x y -> (z y) x
;
;
; a + b  = \f x -> (a f) (b f x)
; (\f x -> f $ f x) + (\f x -> f x)
; \f x -> ((\f x -> f $ f x) f) ((\f x -> f x) f x)
; \f x -> ((\f x -> f $ f x) f) (f x)
; \f x -> (\x -> f $ f x) (f x)
; \f x -> f $ f (f x)
; \f x -> f $ f $ f x
