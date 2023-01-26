#lang racket

(require "../math.rkt")

(provide make-rat numer denom add-rat sub-rat
         mul-rat div-rat equal-rat? print-rat)

(define (make-rat n d)
  (let ((normalize-sign? (or (and (< n 0) (< d 0))
                             (and (> n 0) (< d 0)))))
    (let ((n (if normalize-sign? (* n (- 1)) n))
          (d (if normalize-sign? (* d (- 1)) d)))
      (let ((g (abs (gcd n d))))
        (cons (/ n g) (/ d g))))))

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
