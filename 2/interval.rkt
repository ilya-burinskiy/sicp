#lang racket

(provide add-interval mul-interval div-interval
         make-interval lower-bound upper-bound)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (define (intersect-zero? interval)
    ; assuming that lower bound is less than upper bound
    (and (negative? (lower-bound interval))
         (positive? (upper-bound interval))))
  (cond ((intersect-zero? x) (error "first interval intersects zero"))
        ((intersect-zero? y) (error "second interval intersects zero"))
        (else (mul-interval x
                            (make-interval (/ 1.0 (lower-bound y))
                                           (/ 1.0 (upper-bound y)))))))

(define (make-interval a b) (cons a b))

(define (lower-bound i) (car i))

(define (upper-bound i) (cdr i))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (upper-bound i) (lower-bound)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((err (* c (/ p 100.0))))
    (make-interval (- c err) (+ c err))))
