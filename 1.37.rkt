#lang racket

(define (cont-frac n d k)
  (define (cont-frac-helper i)
    (cond ((< i k)
           (/ (n i)
              (+ (d i) (cont-frac-helper (+ i 1)))))
          ((= i k)
           (/ (n k) (d k)))))
  (cont-frac-helper 1))

(define (cont-frac-iterative n d k)
  (define (cont-frac-helper i result)
    (if (= i 0)
        result
        (cont-frac-helper (- i 1)
                          (/ (n i) (+ (d i) result)))))
  (cont-frac-helper k 0))
