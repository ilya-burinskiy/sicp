#lang racket

(require "sequence.rkt")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list null)
        (filter
          (lambda (positions) (safe? positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (stream->list (in-range 1 (+ board-size 1)))))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (append (list (make-position new-row k)) rest-of-queens))

; Assumes, that queen on k vertical position is first in positions
(define (safe? positions)
  (let ((queen-on-k-vertical (car positions))
        (rest-queens (cdr positions)))
    (accumulate
      (lambda (x y) (and x y))
      true
      (map (lambda (queen) (not (hits? queen queen-on-k-vertical)))
           rest-queens))))

(define (hits? queen-1 queen-2)
  (or (hits-with-horisontal? queen-1 queen-2)
      (hits-with-vertical? queen-1 queen-2)
      (hits-with-diagonal? queen-1 queen-2)))

(define (hits-with-horisontal? queen-1 queen-2)
  (= (horisontal-position queen-1)
     (horisontal-position queen-2)))

(define (hits-with-vertical? queen-1 queen-2)
  (= (vertical-position queen-1)
     (vertical-position queen-2)))

(define (hits-with-diagonal? queen-1 queen-2)
  (= (abs (- (vertical-position queen-1) (vertical-position queen-2)))
     (abs (- (horisontal-position queen-1) (horisontal-position queen-2)))))

(define (make-position i j) (list i j))

(define (vertical-position position) (cadr position))

(define (horisontal-position position) (car position))
