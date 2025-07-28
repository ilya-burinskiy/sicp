#lang racket

(provide
  make-empty-prior-queue
  prior-queue-empty?
  prior-queue-insert
  prior-queue-merge
  prior-queue-min
  prior-queue-pop-min)

(define (prior-queue-rank q)
  (if (prior-queue-empty? q)
      0
      (node-rank q)))
(define (make-empty-prior-queue) null)
(define (prior-queue-empty? queue) (null? queue))
(define (prior-queue-merge q1 q2)
  (cond
    [(prior-queue-empty? q1) q2]
    [(prior-queue-empty? q2) q1]
    [else
      (let ([q1-key (node-key q1)]
            [q1-val (node-val q1)]
            [q1-left (node-left q1)]
            [q1-right (node-right q1)]

            [q2-key (node-key q2)]
            [q2-val (node-val q2)]
            [q2-left (node-left q2)]
            [q2-right (node-right q2)])
        (if (<= q1-key q2-key)
            (make-pqueue-node
              q1-key
              q1-val
              q1-left
              (prior-queue-merge q1-right q2))
            (make-pqueue-node
              q2-key
              q2-val
              q2-left
              (prior-queue-merge q1 q2-right))))]))
(define (make-pqueue-node key val left right)
  (let ([l-rank (prior-queue-rank left)]
        [r-rank (prior-queue-rank right)])
    (if (>= l-rank r-rank)
        (make-node (+ r-rank 1) key val left right)
        (make-node (+ l-rank 1) key val right left))))
(define (prior-queue-insert key val queue)
  (prior-queue-merge
    (make-node 1 key val null null)
    queue))
(define (prior-queue-min q)
  (if (prior-queue-empty? q)
      (error "Queue is empty -- PRIOR-QUEUE-MIN")
      (cons (node-key q) (node-val q))))
(define (prior-queue-pop-min q)
  (if (prior-queue-empty? q)
      (error "Queue is empty -- PRIOR-QUEUE-POP-MIN")
      (prior-queue-merge (node-left q) (node-right q))))

(define (make-node rank key val left right) (list rank key val left right))
(define (node-rank node) (car node))
(define (node-key node) (car (cdr node)))
(define (node-val node) (car (cdr (cdr node))))
(define (node-left node) (car (cdr (cdr (cdr node)))))
(define (node-right node) (car (cdr (cdr (cdr (cdr node))))))
