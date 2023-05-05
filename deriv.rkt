#lang racket

; (define (deriv expr var)
;   (cond ((number? expr) 0)
;         ((variable? expr)
;          (if (same-variable? expr var) 1 0))
;         ((sum? expr)
;          (make-sum (deriv (addend expr) var)
;                    (deriv (augend expr) var)))
;         ((product? expr)
;           (make-sum
;             (make-product (multiplier expr)
;                           (deriv (multiplicand expr) var))
;             (make-product (deriv (multiplier expr) var)
;                           (multiplicand expr))))
;         ((exponentiation? expr)
;          (make-product (make-product (exponent expr)
;                                      (make-exponentiation (base expr)
;                                                           (make-sum (exponent expr) -1)))
;                        (deriv (base expr) var)))
;         (else
;           (error "Unknown expression type -- DERIV" expr))))

(define (install-deriv-package)
  (define (sum-deriv operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  (define (product-deriv operands var)
    (make-sum
      (make-product (multiplier operands)
                    (deriv (multiplicand operands) var))
      (make-product (deriv (multiplier operands) var)
                    (multiplicand operands))))
  (define (exponent-deriv operands var)
    (make-product
      (make-product
        (exponent operands)
        (make-exponentiation (base operands) (make-sum (exponent operands) -1)))
      (deriv (base operands) var)))

  (put 'deriv '+ sum-deriv)
  (put 'deriv '* product-deriv)
  (put 'deriv '** exponent-deriv))

(define (deriv expr var)
  (cond [(number? expr) 0]
        [(variable? expr) (if (same-variable? expr var) 1 0)]
        [else ((get 'deriv (operator expr)) (operands expr) var)]))

(define (operator expr) (car expr))

(define (operands expr) (cdr expr))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (sum? expr) (and (pair? expr) (eq? (car expr) '+)))

(define (addend sum) (car sum))

(define (augend sum-operands)
  (cond
    [(= (length sum-operands) 2) (cadr sum-operands)] ; '(x1 x2)
    [(= (length sum-operands) 3) (make-sum (cadr sum-operands) (caddr sum-operands))] ; '(x1 x2 x3)
    [else (make-sum (cadr sum-operands) (cddr sum-operands))])) ; '(x1 x2 x3 ...)

(define (product? expr) (and (pair? expr) (eq? (car expr) '*)))

(define (multiplier prod) (car prod))

(define (multiplicand prod-operands)
  (cond
    [(= (length prod-operands) 2) (cadr prod-operands)] ; '(x1 x2)
    [(= (length prod-operands) 3) (make-product (cadr prod-operands) (caddr prod-operands))] ; '(x1 x2 x3)
    [else (make-product (cadr prod-operands) (cddr prod-operands))])) ; '(x1 x2 x2 ...)

(define (exponentiation? expr)
  (and (pair? expr) (eq? (car expr) '**)))

(define (base exponent-operands) (car exponent-operands))

(define (exponent exponent-operands) (cadr exponent-operands))

(define (=number? expr num)
  (and (number? expr) (= expr num)))

; TODO: put in a separate module

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Invalid tagged data" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Invalid tagged data" datum)))

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
  (hash-ref op-table (list op type) '()))
