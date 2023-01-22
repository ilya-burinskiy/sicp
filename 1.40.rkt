#lang racket

(define (cubic a b c)
  (define (square x) (* x x))
  (define (cube x) (* x (square x)))
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

(define (deriv g)
  (define dx 1.e-5)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (define tolerance 1e-5)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
