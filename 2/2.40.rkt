#lang racket

(require "sequence.rkt")
(require "../math.rkt")

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  ; (enumerate-interval 1 (- i 1))
                  (sequence->list (in-range 1 i))))
           (sequence->list (in-range 1 (+ n 1)))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))
