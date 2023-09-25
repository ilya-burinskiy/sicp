#lang racket

(require "../type-dispatch.rkt"
         "../math.rkt")

(provide install-rectangular-package)

(define (install-rectangular-package)
  (put 'make-from-real-imag 'rectangular make-from-real-imag)
  (put 'make-from-mag-ang 'rectangular make-from-mag-ang)
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  'done)

(define (tag x) (attach-tag 'rectangular x))
(define (make-from-real-imag x y) (tag (cons x y)))
(define (make-from-mag-ang r a) (tag (cons (* r (cos a)) (* r (sin a)))))
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))
(define (angle z) (atan (imag-part z) (real-part z)))
