#lang racket

(require "type-dispatch.rkt" "arithm.rkt")

(provide install-polynomial-package make-polynomial)

(define (install-polynomial-package)
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =poly-zero?)
  (put 'neg '(polynomial) (lambda (p) (tag (neg-poly p)))))

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polynomials in different variables" (list p1 p2))))

(define (sub-poly p1 p2)
  (add-poly p1 (neg-poly p2)))

(define (neg-poly p)
  (make-poly (variable p) (neg-terms (term-list p))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polynomials in different variables" (list p1 p2))))

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

(define (neg-terms L)
  (map (lambda (term) (make-term (order term) (- (coeff term)))) L))

(define (same-variable? v1 v2)
  ; TODO: copy from deriv.rkt
  (define (variable? x) (symbol? x))

  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (variable p) (car p))

(define (term-list p) (cdr p))

(define (make-poly variable term-list)
  (cons variable term-list))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-term-list) '())

(define (first-term term-list) (car term-list))

(define (rest-terms term-list) (cdr term-list))

(define (empty-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff))

(define (order term) (car term))

(define (coeff term) (cadr term))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (=poly-zero? L)
  (define (iter terms all-zero?)
    (if (or (empty-termlist? terms) (not all-zero?))
        all-zero?
        (let [(term (first-term terms))]
          (iter (rest-terms terms) (and (=zero? (coeff term)) all-zero?)))))
  (let [(terms (term-list L))]
    (iter terms true)))
