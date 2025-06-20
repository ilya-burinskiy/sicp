#lang racket

(define (make-table same-key?)
  (let ([local-table (mcons '*table* '())])
    (define (lookup key)
      (let ([record (assoc key (mcdr local-table))])
        (if record
            (mcdr record)
            false)))
    (define (assoc key records)
      (cond
        [(null? records) false]
        [(same-key? key (mcar (mcar records))) (mcar records)]
        [else (assoc key (mcdr records))]))
    (define (insert! key value)
      (let ([record (assoc key (mcdr local-table))])
        (if record
            (set-mcdr! record value)
            (set-mcdr! local-table
                       (mcons (mcons key value)
                              (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond
        [(eq? m 'insert!) insert!]
        [(eq? m 'lookup) lookup]
        [else (error "Undefined method -- MAKE-TABLE" m)]))
    dispatch))