#lang racket

(require "../type-dispatch.rkt"
         "../math.rkt")

(provide install-polar-package)

(define (install-polar-package)
  (put 'make-from-real-imag 'polar make-from-real-imag)
  (put 'make-from-mag-ang 'polar make-from-mag-ang)
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  'done)

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
