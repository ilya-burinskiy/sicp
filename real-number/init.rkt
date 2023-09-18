#lang racket

(require "../type-dispatch.rkt" "impl.rkt")

(provide install-real-number-package)

(define (install-real-number-package)
  (put 'make 'real-number make-real-number)
  (put 'add '(real-number real-number) add)
  (put 'sub '(real-number real-number) sub)
  (put 'mul '(real-number real-number) mul)
  (put 'div '(real-number real-number) div)
  (put 'equ? '(real-number real-number) equ?)
  (put '=zero? '(real-number) =zero?)
  'done)
