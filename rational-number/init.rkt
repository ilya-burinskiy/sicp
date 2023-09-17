#lang racket

(require "../type-dispatch.rkt" "impl.rkt")

(provide install-rational-package)

(define (install-rational-package)
  (put 'make 'rational make-rat)
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'add '(rational rational) add)
  (put 'sub '(rational rational) sub)
  (put 'mul '(rational rational) mul)
  (put 'div '(rational rational) div)
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  'done)
