#lang racket

(require "../type-dispatch.rkt" "../arithm.rkt")

(provide install-sparse-term-package make-sparse-term)

(define (install-sparse-term-package)
  (put 'make 'sparse-term make-sparse-term)
  (put 'add '(sparse-term sparse-term)
       (lambda (L1 L2) (tag (add-terms L1 L2))))
  (put 'mul '(sparse-term sparse-term)
       (lambda (L1 L2) (tag (mul-terms L1 L2))))
  (put 'sub '(sparse-term sparse-term)
       (lambda (L1 L2) (tag (sub-terms L1 L2))))
  (put 'neg '(sparse-term) (lambda (L) (tag (neg-terms L))))
  (put '=zero? '(sparse-term) =zero-term?))

(define (tag x) (attach-tag 'sparse-term x))

(define (make-sparse-term coeffs-and-orders) (tag coeffs-and-orders))

(define (add-terms L1 L2)
  (cond [(empty-termlist? L1) L2]
        [(empty-termlist? L2) L1]
        [else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond [(> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2))]
                 [(< (order t1) (order t2))
                  (adjoin-term t2 (add-terms L1 (rest-terms L2)))]
                 [else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))]))]))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-term-list)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-term-list)
      (let [(t2 (first-term L))]
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (make-term order coeff) (list order coeff))

(define (sub-terms L1 L2)
  (add-terms L1 (neg-terms L2)))

(define (neg-terms L)
  (map (lambda (term) (make-term (order term) (- (coeff term)))) L))

(define (=zero-term? L)
  (define (iter terms all-zero?)
    (if (or (empty-termlist? terms) (not all-zero?))
        all-zero?
        (let [(term (first-term terms))]
          (iter (rest-terms terms) (and (=zero? (coeff term)) all-zero?)))))
  (iter L true))

(define (empty-termlist? term-list) (null? term-list))

(define (the-empty-term-list) '())

(define (first-term term-list) (car term-list))

(define (rest-terms term-list) (cdr term-list))

(define (order term) (car term))

(define (coeff term) (cadr term))
