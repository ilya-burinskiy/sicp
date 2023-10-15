#lang racket

(require "../type-dispatch.rkt"
         "polar-impl.rkt"
         "rectangular-impl.rkt")

(provide install-complex-package
         make-complex-from-real-imag
         make-complex-from-mag-ang)

(install-rectangular-package)
(install-polar-package)

(define (install-complex-package)
  (put 'make-from-real-imag 'complex make-complex-from-real-imag)
  (put 'make-from-mag-ang 'complex make-complex-from-mag-ang)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex) add)
  (put 'sub '(complex complex) sub)
  (put 'mul '(complex complex) mul)
  (put 'div '(complex complex) div)
  (put 'equ? '(complex) equ?)
  (put '=zero? '(complex) =zero?)
  (put 'neg '(complex) neg)
  'done)

(define (make-complex-from-real-imag x y)
  (tag (make-from-real-imag x y)))

(define (make-complex-from-mag-ang r a)
  (tag (make-from-mag-ang r a)))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (tag z) (attach-tag 'complex z))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (neg z) (tag (apply-generic 'neg z)))

(define (add z1 z2)
  (make-complex-from-real-imag (+ (real-part z1) (real-part z2))
                               (+ (imag-part z1) (imag-part z2))))

(define (sub z1 z2)
  (make-complex-from-real-imag (- (real-part z1) (real-part z2)
                                  (imag-part z1) (imag-part z2))))

(define (mul z1 z2)
  (make-complex-from-mag-ang (* (magnitude z1) (magnitude z2))
                             (+ (angle z1) (angle z2))))
(define (div z1 z2)
  (make-complex-from-mag-ang (/ (magnitude z1) (magnitude z2))
                             (- (angle z1) (angle z2))))
(define (equ? z1 z2)
  (let [(eps 1e-4)]
    (and (<= (abs (- (real-part z1) (real-part z2))) eps)
         (<= (abs (- (imag-part z1) (imag-part z2))) eps))))

(define (=zero? z)
  (= (real-part z) (imag-part z) 0))
