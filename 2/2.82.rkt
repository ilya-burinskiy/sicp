#lang racket

(provide attach-tag
         type-tag
         contents
         apply-generic
         put get
         put-coercion get-coercion)

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
          (let [(casted-to-one-type (try-to-cast-to-one-type type-tags type-tags args))]
            (if (null? casted-to-one-type)
                (error "No methods for this types" (list op type-tags))
                (apply-generic op casted-to-one-type)))))))

(define op-table (make-hash))

(define coercion-table (make-hash))

(define (put op type proc)
  (hash-set! op-table (list op type) proc))

(define (get op type)
  (hash-ref op-table (list op type) false))

(define (put-coercion source-type target-type proc)
  (hash-set! coercion-table (list source-type target-type) proc))

(define (get-coercion source-type target-type)
  (hash-ref coercion-table (list source-type target-type) false))

(define (generate-type-casts source-type target-types)
  (map
    (lambda (target-type)
      (get-coercion source-type target-type)) target-types))

(define (zip-with f xs ys)
  (cond [(or (null? xs) (null? ys)) '()]
        [else
          (cons (f (car xs) (car ys))
                (zip-with f (cdr xs) (cdr ys)))]))

(define (apply-type-casts type-casts xs)
  (zip-with
    (lambda (type-cast value)
      (if type-cast
          (type-cast value)
          '()))
    type-casts
    xs))

(define (try-to-cast-to-one-type rest-of-types all-types xs)
  (if (null? rest-of-types)
      '()
       (let [(source-type (car rest-of-types))]
         (let [(casts-to-source-type (generate-type-casts source-type all-types))]
           (let [(applied-type-casts (apply-type-casts casts-to-source-type xs))]
             (printf "Applied type casts ~a\n" applied-type-casts)
             (if (ormap null? applied-type-casts)
                 (try-to-cast-to-one-type (cdr rest-of-types) all-types xs)
                 applied-type-casts))))))

