#lang racket

(define (make-table)
  (let ([local-table (mcons '*table* '())])
    (define (lookup . keys)
      (define (iter keys records)
        (if (not (null? keys))
          (let* ([key (car keys)]
                 [subtable (assoc key records)])
            (if subtable
                '()
                '()))
          false))
      (iter keys (mcdr local-table)))))