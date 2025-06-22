#lang racket

(define (make-table)
  (let ([local-table (mcons '*table* '())])
    (define (lookup keys)
      (define (iter keys records)
        (let ([key (car keys)]
              [other-keys (cdr keys)])
          (if (not (null? other-keys))
              (let ([subtable (assoc key records)])
                (if subtable
                    (iter other-keys (mcdr subtable))
                    false))
              (let ([record (assoc key records)])
                (if record
                    (mcdr record)
                    false)))))
      (if (not (null? keys))
          (iter keys (mcdr local-table))
          false))
    (define (assoc key records)
      (cond
        [(null? records) false]
        [(eq? key (mcar (mcar records))) (mcar records)]
        [else (assoc key (mcdr records))]))
    (define (insert! keys value)
      (define (iter keys table)
        (let ([key (car keys)]
              [other-keys (cdr keys)])
          (if (not (null? other-keys))
              (let ([subtable (assoc key (mcdr table))])
                (if subtable
                    (if (pair? subtable)
                        (iter other-keys subtable)
                        (let ([subtable (mcons key '())])
                          (begin
                            (set-mcdr! table (mcons subtable (mcdr table)))
                            (iter other-keys subtable))))
                    (let ([subtable (mcons key '())])
                      (begin
                        (set-mcdr! table (mcons subtable (mcdr table)))
                        (iter other-keys subtable)))))
              (let ([record (assoc key (mcdr table))])
                (if record
                    (set-mcdr! record value)
                    (set-mcdr!
                      table
                      (mcons (mcons key value) (mcdr table))))))))
      (if (not (null? keys))
          (iter keys local-table)
          (error "Provide at least one key -- INSERT!")))
    (define (dispatch m)
      (cond
        [(eq? m 'insert!) insert!]
        [(eq? m 'lookup) lookup]
        [else (error "Undefined method -- MAKE-TABLE" m)]))
    dispatch))
