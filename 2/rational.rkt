#lang racket

(require "../math.rkt")

(provide make-rat numer denom add-rat sub-rat
         mul-rat div-rat equal-rat? print-rat)

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat r1 r2)
  (make-rat (+ (* (numer r1) (denom r2))
               (* (numer r2) (denom r1)))
            (* (denom r1) (denom r2))))

(define (sub-rat r1 r2)
  (make-rat (- (* (numer r1) (denom r2))
               (* (numer r2) (denom r1)))
            (* (denom r1) (denom r2))))

(define (mul-rat r1 r2)
  (make-rat (* (numer r1) (numer r2))
            (* (denom r1) (denom r2))))

(define (div-rat r1 r2)
  (make-rat (* (numer r1) (denom r2))
            (* (denom r1) (numer r2))))

(define (equal-rat? r1 r2)
  (= (* (numer r1) (denom r2))
     (* (numer r2) (denom r1))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))
