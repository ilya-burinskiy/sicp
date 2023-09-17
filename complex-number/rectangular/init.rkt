#lang racket

(require "../../type-dispatch.rkt" "impl.rkt")

(provide install-rectangular-package)

(define (install-rectangular-package)
  (put 'make-from-real-imag 'rectangular make-from-real-imag)
  (put 'make-from-mag-ang 'rectangular make-from-mag-ang)
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  'done)
