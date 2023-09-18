#lang racket

(require "../type-dispatch.rkt" "impl.rkt")

(provide install-integer-number-package)

(define (install-integer-number-package)
  (put 'make 'integer-number make-integer-number)
  (put 'add '(integer-number integer-number) add)
  (put 'sub '(integer-number integer-number) sub)
  (put 'mul '(integer-number integer-number) mul)
  (put 'div '(integer-number integer-number) div)
  (put 'equ? '(integer-number integer-number) equ?)
  (put '=zero? '(integer-number) =zero?)
  'done)
