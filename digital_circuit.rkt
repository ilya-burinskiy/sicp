#lang racket

(provide
  make-wire
  get-signal
  set-signal!
  add-action!
  inverter
  and-gate
  or-gate
  half-adder
  full-adder)

(define (make-wire) '())
(define (after-delay t callaback) '())
(define (get-signal wire) '())
(define (set-signal! wire) '())
(define (add-action! wire action) '())

(define inverter-delay 1)
(define and-gate-delay 1)
(define or-gate-delay 1)

(define (inverter input output)
  (define (invert-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay
        inverter-delay
        (lambda ()
          (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond
    [(= s 0) 1]
    [(= s 1) 0]
    [else (error "Invalid signal" s)]))

(define (and-gate a1 a2 output)
  (define (and-action-proc)
    (let ([new-val
            (logical-and (get-signal a1)
                         (get-signal a2))])
      (after-delay
        and-gate-delay
        (lambda ()
          (set-signal! output new-val)))))
  (add-action! a1 and-action-proc)
  (add-action! a2 and-action-proc)
  'ok)

(define (logical-and s1 s2)
  (cond
    [(= s1 s2 1) 1]
    [(= s1 s2 0) 0]
    [(or (and (= s1 1) (= s2 0))
         (and (= s1 0) (= s2 1)))
     0]
    [else "Invalid signals" s1 s2]))

(define (half-adder a b s c)
  (let ([d (make-wire)]
        [e (make-wire)])
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ([s (make-wire)]
        [c1 (make-wire)]
        [c2 (make-wire)])
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (or-gate a1 a2 output)
  (define (or-action-proc)
    (let ([new-val
            (logical-or (get-signal a1)
                        (get-signal a2))])
      (after-delay
        or-gate-delay
        (lambda ()
          (set-signal! output new-val)))))
  (add-action! a1 or-action-proc)
  (add-action! a2 or-action-proc)
  'ok)

(define (logical-or s1 s2)
  (cond
    [(= s1 s2 1) 1]
    [(= s1 s2 0) 0]
    [(or (and (= s1 1) (= s2 0))
         (and (= s1 0) (= s2 1)))
     1]
    [else (error "Invalid signals" s1 s2)]))
