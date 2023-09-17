#lang racket

(require "../type-dispatch.rkt" "impl.rkt")

(provide install-complex-package)

(define (install-complex-package)
  (put 'make-from-real-imag 'complex make-from-real-imag)
  (put 'make-from-mag-ang 'complex make-from-mag-ang)
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
  'done)
