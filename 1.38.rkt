#lang racket

(define (cont-frac-iterative n d k)
  (define (cont-frac-helper i result)
    (if (= i 0)
        result
        (cont-frac-helper (- i 1)
                          (/ (n i) (+ (d i) result)))))
  (cont-frac-helper k 0))

(define (d i)
  (cond ((= i 2) 2.0)
        ((or (= (remainder i 3) 0)
              (= (remainder i 3) 1)) 1.0)
        ((= (remainder i 3) 2) (* 2.0 (+ (floor (/ i 3)) 1)))))

(define (e-cont-frac k)
  (cont-frac-iterative (lambda (i) 1.0) d k))
