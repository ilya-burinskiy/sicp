#lang racket

(require "integer-number/init.rkt"
         "rational-number/init.rkt"
         "real-number/init.rkt"
         "complex-number/init.rkt")

(provide install-arithm-package)

(define (install-arithm-package)
  (install-integer-number-package)
  (install-rational-package)
  (install-real-number-package)
  (install-complex-package)
  'done)
