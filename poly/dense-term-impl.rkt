#lang racket

(require "../arithm.rkt" "../type-dispatch.rkt")

(provide install-dense-term-package make-dense-term)

(define (install-dense-term-package)
  (put 'make 'dense-term make-dense-term)
  (put 'add '(dense-term dense-term)
       (lambda (L1 L2) (tag (add-terms L1 L2))))
  (put 'mul '(dense-term dense-term)
       (lambda (L1 L2) (tag (mul-terms L1 L2))))
  (put 'sub '(dense-term dense-term)
       (lambda (L1 L2) (tag (sub-terms L1 L2))))
  (put 'neg '(dense-term) (lambda (L) (tag (neg-terms L))))
  (put '=zero? '(dense-term) =zero-term?))

(define (tag x) (attach-tag 'dense-term x))

(define (make-dense-term coeffs) (tag coeffs))

(define (add-terms L1 L2)
  (define (iter L1-reversed L2-reversed result)
    (cond [(and (not (null? L1-reversed)) (not (null? L2-reversed)))
           (let [(t1 (car L1-reversed))
                 (t2 (car L2-reversed))]
             (iter (cdr L1-reversed)
                   (cdr L2-reversed)
                   (cons (add t1 t2) result)))]
          [(and (not (null? L1-reversed)) (null? L2-reversed))
           (iter (cdr L1-reversed)
                 L2-reversed
                 (cons (car L1-reversed) result))]
          [(and (not (null? L2-reversed)) (null? L1-reversed))
           (iter L1-reversed
                 (cdr L2-reversed)
                 (cons (car L2-reversed) result))]
          [else result]))
  (cond [(empty-termlist? L1) L2]
        [(empty-termlist? L2) L1]
        [else (iter (reverse L1) (reverse L2) (the-empty-term-list))]))

(define (mul-terms L1 L2)
  (define (iter L1-reversed t1-order L2-reversed result)
    (if (empty-termlist? L1-reversed)
        result
        (iter
         (rest-terms L1-reversed)
         (+ t1-order 1)
         L2-reversed
         (add-terms
          (mul-term-by-all-terms (first-term L1-reversed)
                                 t1-order
                                 L2-reversed)
          result))))
  (if (empty-termlist? L1)
      (the-empty-term-list)
      (iter (reverse L1) 0 (reverse L2) (the-empty-term-list))))

(define (mul-term-by-all-terms t1-coeff t1-order L-reversed)
  (define (shift-by count result)
    (if (<= count 0)
        result
        (shift-by (- count 1) (cons 0 result))))
  (define (iter L-reversed result)
    (if (empty-termlist? L-reversed)
        result
        (let [(t2-coeff (car L-reversed))]
          (iter (cdr L-reversed) (cons (mul t1-coeff t2-coeff) result)))))
  (iter (shift-by t1-order L-reversed) (the-empty-term-list)))

(define (sub-terms L1 L2)
  (add-terms L1 (neg-terms L2)))

(define (neg-terms L) (map - L))

(define (=zero-term? L)
  (define (iter terms all-zero?)
    (if (or (empty-termlist? terms) (not all-zero?))
        all-zero?
        (iter (rest-terms terms) (and (=zero? (first-term terms)) (all-zero?)))))
  (iter L true))

(define (empty-termlist? L) (null? L))

(define (the-empty-term-list) '())

(define (first-term L) (car L))

(define (rest-terms L) (cdr L))
