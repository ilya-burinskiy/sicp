#lang racket

(define (make-segment start end) (cons start end))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (midpoint-segment s)
  (let ((x1 (x-point (start-segment s)))
        (y1 (y-point (start-segment s)))
        (x2 (x-point (end-segment s)))
        (y2 (y-point (end-segment s))))
    (let ((xc (/ (+ x1 x2) 2))
          (yc (/ (+ y1 y2) 2)))
      (make-point xc yc))))

(define (make-rectangle p1 p2)
  (cons p1 p2))

(define (rectangle-p1 rect) (car rect))

(define (rectangle-p2 rect) (cdr rect))

(define (rectangle-area rect)
  (let ((p1 (rectangle-p1 rect))
        (p2 (rectangle-p2 rect)))
    (let ((a (- (x-point p2) (x-point p1)))
          (b (- (y-point p2) (y-point p1))))
      (* a b))))

(define (rectangle-perimeter rect)
  (let ((p1 (rectangle-p1 rect))
        (p2 (rectangle-p2 rect)))
    (let ((a (- (x-point p2) (x-point p1)))
          (b (- (y-point p2) (y-point p1))))
      (* 2 (+ a b)))))
