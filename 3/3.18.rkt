#lang racket

(define (has-cycle? head)
  (define seen '())
  (define (iter head)
    (if (mpair? head)
        (if (not (memq head seen))
            (begin
              (set! seen (cons head seen))
              (or
                (iter (mcar head))
                (iter (mcdr head))))
            #t)
        #f))
  (iter head))
