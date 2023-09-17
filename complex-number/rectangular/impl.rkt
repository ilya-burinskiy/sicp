#lang racket

(require "../../type-dispatch.rkt" "../../math.rkt")

(provide make-from-real-imag
         make-from-mag-ang
         real-part
         imag-part
         magnitude
         angle)

(define (tag x) (attach-tag 'rectangular x))
(define (make-from-real-imag x y) (tag (cons x y)))
(define (make-from-mag-ang r a) (tag (cons (* r (cos a)) (* r (sin a)))))
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))
(define (angle z) (atan (imag-part z) (real-part z)))
