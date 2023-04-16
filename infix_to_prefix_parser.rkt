#lang racket

(require "stack.rkt")

(provide infix->prefix)

(define (infix->prefix expr)
  (car
    (parse-result
      (parse-expr-with-priority-3-operators
        (make-parser-state '() expr)))))

(define (parse-expr-with-priority-3-operators parser-state)
  (parse-rest-of-expr-with-priority-3-operators
    (parse-expr-with-priority-2-operators parser-state)))

(define (parse-rest-of-expr-with-priority-3-operators parser-state)
  (cond ((null? (parsing-string parser-state)) parser-state)
        ((eq? (car (parsing-string parser-state)) '+)
         (let
           ((state-after-parsing-second-operand
              (parse-expr-with-priority-2-operators
                 (make-parser-state
                   (parse-result parser-state)
                   (cdr (parsing-string parser-state))))))
           (parse-rest-of-expr-with-priority-3-operators
             (make-prefix-binop state-after-parsing-second-operand '+))))
        ((eq? (car (parsing-string parser-state)) '-)
         (let
           ((state-after-parsing-second-operand
             (parse-expr-with-priority-2-operators
               (make-parser-state
                 (parse-result parser-state)
                 (cdr (parsing-string parser-state))))))
           (parse-rest-of-expr-with-priority-3-operators
             (make-prefix-binop state-after-parsing-second-operand '-))))
        (else parser-state)))

(define (parse-expr-with-priority-2-operators parser-state)
  (parse-rest-of-expr-with-priority-2-operators
    (parse-expr-with-priority-1-operators parser-state)))

(define (parse-rest-of-expr-with-priority-2-operators parser-state)
  (cond ((null? (parsing-string parser-state)) parser-state)
        ((eq? (car (parsing-string parser-state)) '*)
         (let
           ((state-after-parsing-second-operand
              (parse-expr-with-priority-1-operators
                 (make-parser-state
                   (parse-result parser-state)
                   (cdr (parsing-string parser-state))))))
           (parse-rest-of-expr-with-priority-2-operators
             (make-prefix-binop state-after-parsing-second-operand '*))))
        ((eq? (car (parsing-string parser-state)) '/)
         (let
           ((state-after-parsing-second-operand
             (parse-expr-with-priority-1-operators
               (make-parser-state
                 (parse-result parser-state)
                 (cdr (parsing-string parser-state))))))
           (parse-rest-of-expr-with-priority-2-operators
             (make-prefix-binop state-after-parsing-second-operand '/))))
        (else parser-state)))

(define (parse-expr-with-priority-1-operators parser-state)
  (let ((state-after-parsing-atom-expr (parse-atom-expr parser-state)))
    (cond ((null? (parsing-string state-after-parsing-atom-expr)) state-after-parsing-atom-expr)
          ((eq? (car (parsing-string state-after-parsing-atom-expr)) '**)
           (let
             ((state-after-parsing-second-operand
               (parse-expr-with-priority-1-operators
                 (make-parser-state
                   (parse-result state-after-parsing-atom-expr)
                   (cdr (parsing-string state-after-parsing-atom-expr))))))
             (make-prefix-binop state-after-parsing-second-operand '**)))
          (else state-after-parsing-atom-expr))))

(define (parse-atom-expr parser-state)
  (cond ((or (number? (car (parsing-string parser-state)))
             (symbol? (car (parsing-string parser-state))))
         (make-parser-state
           (push-on-stack (parse-result parser-state) (car (parsing-string parser-state)))
           (cdr (parsing-string parser-state))))
        ((pair? (car (parsing-string parser-state)))
         (make-parser-state
           (parse-result
             (parse-expr-with-priority-3-operators
               (make-parser-state
                 (parse-result parser-state)
                 (car (parsing-string parser-state)))))
           (cdr (parsing-string parser-state))))
        (else (error "syntax error"))))

(define (make-prefix-binop parser-state op)
  (define stack-pop-result-1 (pop-from-stack (parse-result parser-state)))
  (define parse-result-without-operand-2 (car stack-pop-result-1))
  (define operand-2 (cadr stack-pop-result-1))

  (define stack-pop-result-2 (pop-from-stack parse-result-without-operand-2))
  (define parse-result-without-operands (car stack-pop-result-2))
  (define operand-1 (cadr stack-pop-result-2))

  (make-parser-state
    (push-on-stack
      parse-result-without-operands
      (list op operand-1 operand-2))
    (parsing-string parser-state)))

(define (make-parser-state result input) (list result input))
(define (parse-result parser-state) (car parser-state))
(define (parsing-string parser-state) (cadr parser-state))
