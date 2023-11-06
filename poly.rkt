#lang racket

(require "type-dispatch.rkt" "arithm.rkt"
         "poly/sparse-term-impl.rkt"
         "poly/dense-term-impl.rkt")

(provide install-polynomial-package make-polynomial)

(install-dense-term-package)
(install-sparse-term-package)

(define (install-polynomial-package)
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (let [(div-poly-result (div-poly p1 p2))]
           (let [(quotient* (car div-poly-result))
                 (remainder* (cadr div-poly-result))]
             (list (tag quotient*) (tag remainder*))))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =poly-zero?)
  (put 'neg '(polynomial) (lambda (p) (tag (neg-poly p)))))

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add (term-list p1)
                      (term-list p2)))
      (error "Polynomials in different variables" (list p1 p2))))

(define (sub-poly p1 p2)
  (add-poly p1 (neg-poly p2)))

(define (neg-poly p)
  (make-poly (variable p) (neg (term-list p))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul (term-list p1)
                      (term-list p2)))
      (error "Polynomials in different variables" (list p1 p2))))

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (let [(div-terms-result (div (term-list p1)
                                   (term-list p2)))]
        (let [(quotient* (car div-terms-result))
              (remainder* (cadr div-terms-result))]
          (list (make-poly (variable p1) quotient*)
                (make-poly (variable p1) remainder*))))
      (error "Polynomials in different variables" (list p1 p2))))

(define (same-variable? v1 v2)
  ; TODO: copy from deriv.rkt
  (define (variable? x) (symbol? x))

  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (variable p) (car p))

(define (term-list p) (cdr p))

(define (make-poly variable term-list)
  (cons variable term-list))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (=poly-zero? L) (=zero? (term-list L)))
