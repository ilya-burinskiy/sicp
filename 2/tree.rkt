#lang racket

(require "sequence.rkt")
(provide tree-map square-tree count-leaves)

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map (lambda (x) (* x x)) tree))

(define (count-leaves tree)
  (accumulate + 0 (map
                    (lambda (sub-tree)
                      (if (pair? sub-tree)
                          (count-leaves sub-tree)
                          1))
                    tree)))
