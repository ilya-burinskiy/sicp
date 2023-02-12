#lang racket

(define (square-list items)
  ; (cons (a_1^2) nil)
  ; (cons (a_2^2) (cons a_1^2 nil))
  ; (cons (a_n^2) (cons a_(n-1)^2 (...(cons (a_1^2) nil)...))
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

(define (square-list items)
  ; (iter (cdr (a_1 a_2 ... a_n)) (cons nil (a_1^2))
  ; (iter (a_2 a_3 ... a_n) (cons nil (a_1^2))
  ; (iter (cdr (a_2 a_3 ... a_n)) (cons (cons nil (a_1^2) (a_2^2))
  ; (iter (a_3 a_4 ... a_n) (cons (cons nil (a_1^2))
  ;                            (a_2^2))
  ; (cons (cons (...(cons nil (a_1^2))...) a_(n-1)^2) (a_n^2))
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter things null))
