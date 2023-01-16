#lang racket

(define (search-for-primes start-range end-range)
  (if (even? start-range)
    (search-for-primes (+ 1 start-range) end-range)
    (when (<= start-range end-range)
      (timed-prime-test start-range)
      (search-for-primes (+ 2 start-range) end-range))))

(define (timed-prime-test n)
  (let ((start-time (current-inexact-milliseconds)))
    (when (prime? n)
      (let ((elapsed-time (- (current-inexact-milliseconds) start-time)))
        (display n)
        (display " *** ")
        (display elapsed-time)
        (newline)))))

(define (square x) (* x x))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
