#lang racket

(require "scheme-number/init.rkt"
         "rational-number/init.rkt"
         "complex-number/init.rkt")

(provide install-arithm-package)

(define (install-arithm-package)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package))
