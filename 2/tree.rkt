#lang racket

(define (square x) (* x x))

(define (square-tree-1 tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-1 (car tree))
                    (square-tree-1 (cdr tree))))))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (square sub-tree)))
       tree))
