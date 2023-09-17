#lang racket

(require "../../type-dispatch.rkt" "../../math.rkt")

(provide make-from-real-imag
         make-from-mag-ang
         real-part
         imag-part
         magnitude
         angle)

(define (tag x) (attach-tag 'polar x))
(define (make-from-real-imag x y)
  (tag (cons (sqrt (+ (square x) (square y)))
             (atan y x))))
(define (make-from-mag-ang r a) (tag (cons r a)))
(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
