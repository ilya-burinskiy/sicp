#lang racket

(provide gcd square find-divisor divides? prime?)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (square x) (* x x))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next divisor)
    (cond ((= divisor 2) 3)
          (else (+ divisor 2))))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
