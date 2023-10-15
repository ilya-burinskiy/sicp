#lang racket

(require "type-dispatch.rkt" "math.rkt")

(provide install-rational-package
         make-rat)

(define (install-rational-package)
  (put 'make 'rational make-rat)
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'add '(rational rational) add)
  (put 'sub '(rational rational) sub)
  (put 'mul '(rational rational) mul)
  (put 'div '(rational rational) div)
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'neg '(rational) neg)
  'done)

(define (make-rat n d)
  (let [(g (gcd n d))]
    (tag (cons (/ n g) (/ d g)))))

(define (tag x) (attach-tag 'rational x))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul x y)
  (make-rat (* (numer x) (numer y))
            (* (denom y) (denom x))))

(define (div x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equ? x y)
  (and (= (numer x) (numer y))
       (= (denom x) (denom y))))

(define (=zero? x) (= (numer x) 0))

(define (neg x)
  (make-rat (- (numer x)) (denom x)))
