#lang racket

(define (element-of-set? x set)
  (cond [(null? set) false]
        [(= x (entry set)) true]
        [(< x (entry set)) (element-of-set? x (left-branch set))]
        [(> x (entry set)) (element-of-set? x (right-branch set))]))

(define (adjoin-set x set)
  (cond [(null? set) (make-tree x '() '())]
        [(= x (entry set)) set]
        [(< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set))]
        [(> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set)))]))

(define (intersection-set set1 set2)
  (list->tree
    (intersection-list-set
      (tree->list set1)
      (tree->list set2))))

(define (union-set set1 set2)
  (list->tree
    (union-list-set
      (tree->list set1)
      (tree->list set2))))

(define (intersection-list-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond [(= x1 x2)
               (cons x1
                     (intersection-list-set (cdr set1)
                                       (cdr set2)))]
              [(< x1 x2) (intersection-list-set (cdr set1) set2)]
              [(> x1 x2) (intersection-list-set set1 (cdr set2))]))))

(define (union-list-set set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [else
          (let ((x1 (car set1))
                (x2 (car set2)))
            (cond [(= x1 x2) (cons x1 (union-list-set (cdr set1) (cdr set2)))]
                  [(< x1 x2) (cons x1 (union-list-set (cdr set1) set2))]
                  [(> x1 x2) (cons x2 (union-list-set set1 (cdr set2)))]))]))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
    (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elements n)
  (if (= n 0)
      (cons '() elements)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elements left-size)))
          (let ((left-tree (car left-result))
                (non-left-elements (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elements))
                  (right-result (partial-tree (cdr non-left-elements)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elements (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elements))))))))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))
