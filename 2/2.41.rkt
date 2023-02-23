#lang racket

(require "sequence.rkt")

(define (unique-triples n)
  (flatmap (lambda (i)
         (flatmap (lambda (j)
                (map (lambda (k) (list i j k))
                     (stream->list (in-range (+ j 1) (+ n 1)))))
              (stream->list (in-range (+ i 1) (+ n 1)))))
       (stream->list (in-range 1 (+ n 1)))))

; Процедура, которая находит все такие упорядоченные тройки различных
; положительных чисел i, j, k, меньших или равных данному числу n, сумма
; которых равна данному числу s
(define (solution n s)
  (filter (lambda (triple)
            (let ((x (car triple))
                  (y (cadr triple))
                  (z (caddr triple)))
              (= (+ x y z) s)))
          (unique-triples n)))
