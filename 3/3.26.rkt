#lang racket

(define (make-entry key val) (cons key val))
(define (entry-key entry) (car entry))
(define (entry-val entry) (cdr entry))

(define (make-tree entry left right) (list entry left right))
(define (empty-tree) '())
(define (empty-tree? tree) (null? tree))
(define (tree-entry tree) (car tree))
(define (tree-left-branch tree) (cadr tree))
(define (tree-right-branch tree) (caddr tree))
(define (tree-lookup k tree)
  (cond
    [(empty-tree? tree) false]
    [(= k (entry-key (tree-entry tree))) (tree-entry tree)]
    [(< k (entry-key (tree-entry tree))) (tree-lookup k (tree-left-branch tree))]
    [(> k (entry-key (tree-entry tree))) (tree-lookup k (tree-right-branch tree))]))

(define (insert e tree)
  (cond
    [(empty-tree? tree) (make-tree e (empty-tree) (empty-tree))]
    [(< (entry-key e) (entry-key (tree-entry tree)))
     (make-tree
       (tree-entry tree)
       (insert e (tree-left-branch tree))
       (tree-right-branch tree))]
    [(> (entry-key e) (entry-key (tree-entry tree)))
     (make-tree
       (tree-entry tree)
       (tree-left-branch tree)
       (insert e (tree-right-branch tree)))]))

(define (make-table)
  (let ([tree (empty-tree)])
    (define (lookup key)
      (let ([res (tree-lookup key tree)])
        (if res
            (cdr res)
            false)))
    (define (insert! key val)
      (set! tree (insert (make-entry key val) tree)))
    (define (dispatch m)
      (cond
        [(eq? m 'insert!) insert!]
        [(eq? m 'lookup) lookup]
        [else (error "Undefined method -- MAKE-TABLE" m)]))
    dispatch))
