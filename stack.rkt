#lang racket

(provide empty-stack push-on-stack pop-from-stack)

(define (empty-stack) '())

(define (push-on-stack stack item) (append stack (list item)))

(define (pop-from-stack stack)
  (define (list-init lst)
    (cond ((null? lst) (error "empty list"))
          ((null? (cdr lst)) '())
          (else (cons (car lst) (list-init (cdr lst))))))

  (if (not (null? stack))
      (list (list-init stack) (last stack))
      (error "empty stack")))
