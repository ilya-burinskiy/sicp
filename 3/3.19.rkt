#lang racket

(define (has-cycle? head)
  (define (go-forward head steps)
    (cond
      [(<= steps 0) head]
      [(null? head) head]
      [else (go-forward (mcdr head) (- steps 1))]))
  (define (iter slow fast)
    (cond
      [(null? fast) #f]
      [(eq? fast slow) #t]
      [else
        (iter
          (go-forward slow 1)
          (go-forward fast 2))]))
  (iter head (go-forward head 2)))
