#lang racket

(provide attach-tag type-tag contents apply-generic put get)

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond [(number? datum) 'scheme-number]
        [(pair? datum) (car datum)]
        [else (error "Invalid tagged data" datum)]))

(define (contents datum)
  (cond [(number? datum) datum]
        [(pair? datum) (cdr datum)]
        [else (error "Invalid tagged data" datum)]))

(define (apply-generic op . args)
  (let [(type-tags (map type-tag args))]
    (let [(proc (get op type-tags))]
      (if proc
          (apply proc (map contents args))
          (error
           "No method fot this types"
           (list op type-tags))))))

(define op-table (make-hash))

(define (put op type proc)
  (hash-set! op-table (list op type) proc))

(define (get op type)
  (hash-ref op-table (list op type) false))
