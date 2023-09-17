#lang racket

(require "../type-dispatch.rkt"
         "polar/init.rkt"
         "rectangular/init.rkt")

(provide make-from-real-imag
         make-from-mag-ang
         real-part
         imag-part
         magnitude
         angle
         add
         sub
         mul
         div
         equ?
         =zero?)

(install-rectangular-package)
(install-polar-package)

(define (make-from-real-imag x y)
  (tag ((get 'make-from-real-imag 'rectangular) x y)))
(define (make-from-mag-ang r a)
  (tag ((get 'make-from-mag-ang 'polar) r a)))
(define (tag z) (attach-tag 'complex z))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (add z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2)
                          (imag-part z1) (imag-part z2))))

(define (mul z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))
(define (equ? z1 z2)
  (let [(eps 1e-4)]
    (and (<= (abs (- (real-part z1) (real-part z2))) eps)
         (<= (abs (- (imag-part z1) (imag-part z2))) eps))))

(define (=zero? z)
  (= (real-part z) (imag-part z) 0))
