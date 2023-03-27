#lang racket

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
          (make-sum
            (make-product (multiplier expr)
                          (deriv (multiplicand expr) var))
            (make-product (deriv (multiplier expr) var)
                          (multiplicand expr))))
        ((exponentiation? expr)
         (make-product (make-product (exponent expr)
                                     (make-exponentiation (base expr)
                                                          (make-sum (exponent expr) -1)))
                       (deriv (base expr) var)))
        (else
          (error "Unknown expression type -- DERIV" expr))))

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

(define (addend sum) (cadr sum))

(define (augend sum) (caddr sum))

(define (product? expr) (and (pair? expr) (eq? (car expr) '*)))

(define (multiplier prod) (cadr prod))

(define (multiplicand prod) (caddr prod))

(define (exponentiation? expr)
  (and (pair? expr) (eq? (car expr) '**)))

(define (base exponentiation) (cadr exponentiation))

(define (exponent exponentiation) (caddr exponentiation))

(define (=number? expr num)
  (and (number? expr) (= expr num)))
