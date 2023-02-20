#lang racket

(require "sequence.rkt")

(define (reverse-1 sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))

(define (reverse-2 sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

; [1, 2, 3]
; 1 `append` (2 `append` (3 `append` [])) = [3, 2, 1]
; 3 : (2 : (1 : [])) = [3, 2, 1]
