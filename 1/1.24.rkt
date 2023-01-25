#lang racket

(define (search-for-primes start-range end-range)
  (if (even? start-range)
    (search-for-primes (+ 1 start-range) end-range)
    (when (<= start-range end-range)
      (timed-prime-test start-range)
      (search-for-primes (+ 2 start-range) end-range))))

(define (timed-prime-test n)
  (let ((start-time (current-inexact-milliseconds)))
    (when (fast-prime? n 5)
      (let ((elapsed-time (- (current-inexact-milliseconds) start-time)))
        (display n)
        (display " *** ")
        (display elapsed-time)
        (newline)))))

(define (expmod  base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (square (expmod base (/ exp 2) m)) m))
        (else
          (remainder (* base (expmod base (- exp 1) m)) m))))

(define (square x)
  (* x x))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
