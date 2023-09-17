#lang racket

(require "../type-dispatch.rkt" "impl.rkt")

(provide install-scheme-number-package)

(define (install-scheme-number-package)
  (put 'make 'scheme-number make-scheme-number)
  (put 'add '(scheme-number scheme-number) add)
  (put 'sub '(scheme-number scheme-number) sub)
  (put 'mul '(scheme-number scheme-number) mul)
  (put 'div '(scheme-number scheme-number) div)
  (put 'equ? '(scheme-number scheme-number) equ?)
  (put '=zero? '(scheme-number) =zero?)
  'done)
