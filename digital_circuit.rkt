#lang racket

(require "queue.rkt")

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

(define inverter-delay 1)
(define and-gate-delay 2)
(define or-gate-delay 2)
(define the-agenda '())

(define (init-agenda) (set! the-agenda (make-agenda)))

; CIRCUITS
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

(define (ripple-carry-adder as bs os c-in)
  (define (iter as bs os c-in)
    (let ([a (car as)]
          [b (car bs)]
          [o (car os)]
          [as-rest (cdr as)]
          [bs-rest (cdr bs)]
          [os-rest (cdr os)])
      (if (not (null? as-rest))
          (let ([c-out (make-wire)])
            (full-adder a b c-in o c-out)
            (iter as-rest bs-rest os-rest c-out))
          (full-adder a b c-in o (make-wire)))))
  (iter as bs os c-in))

; PRIMITIVES
(define (make-wire)
  (let ([signal-val 0]
        [action-procs '()])
    (define (set-signal! new-val)
      (if (not (= signal-val new-val))
          (begin (set! signal-val new-val)
                 (call-each action-procs))
          'done))
    (define (add-action! proc)
      (set! action-procs (cons proc action-procs))
      (proc))
    (define (dispatch m)
      (cond
        [(eq? m 'get-signal) signal-val]
        [(eq? m 'set-signal!) set-signal!]
        [(eq? m 'add-action!) add-action!]
        [else "Unknown method -- WIRE" m]))
    dispatch))
(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-val) ((wire 'set-signal!) new-val))
(define (add-action! wire action) ((wire 'add-action!) action))

(define (inverter input output)
  (define (invert-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay
        inverter-delay
        (lambda ()
          (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

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

; LOGIC FUNCS
(define (logical-or s1 s2)
  (cond
    [(= s1 s2 1) 1]
    [(= s1 s2 0) 0]
    [(or (and (= s1 1) (= s2 0))
         (and (= s1 0) (= s2 1)))
     1]
    [else (error "Invalid signals" s1 s2)]))

(define (logical-not s)
  (cond
    [(= s 0) 1]
    [(= s 1) 0]
    [else (error "Invalid signal" s)]))

(define (logical-and s1 s2)
  (cond
    [(= s1 s2 1) 1]
    [(= s1 s2 0) 0]
    [(or (and (= s1 1) (= s2 0))
         (and (= s1 0) (= s2 1)))
     0]
    [else "Invalid signals" s1 s2]))

; AGENDA
(define (make-agenda) (mcons 0 '()))
(define (current-time agenda) (mcar agenda))
(define (set-current-time! agenda time) (set-mcar! agenda time))
(define (segments agenda) (mcdr agenda))
(define (set-segments! agenda segments) (set-mcdr! agenda segments))
(define (first-segment agenda) (mcar (segments agenda)))
(define (rest-segments agenda) (mcdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))
  (define (make-new-time-segment time action)
    (let ([q (make-queue)])
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
        (insert-queue! (segment-queue (mcar segments))
                       action)
        (let ([rest (mcdr segments)])
          (if (belongs-before? rest)
              (set-mcdr!
                segments
                (mcons (make-new-time-segment time action)
                       (mcdr segments)))
              (add-to-segments! rest)))))
  (let ([segments (segments agenda)])
    (if (belongs-before? segments)
        (set-segments!
          agenda
          (mcons (make-new-time-segment time action)
                 segments))
        (add-to-segments! segments))))
(define (remove-first-agenda-item! agenda)
  (let ([q (segment-queue (first-segment agenda))])
    (delete-queue! q)
    (when (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))
(define (first-agenda-item agenda)
  (if (not (empty-agenda? agenda))
      (let ([first-seg (first-segment agenda)])
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")))
(define (after-delay t callback)
  (add-to-agenda! (+ t (current-time the-agenda))
                  callback
                  the-agenda))
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ([fst-item (first-agenda-item the-agenda)])
        (fst-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (make-time-segment time queue) (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

; UTILS
(define (call-each procs)
  (if (null? procs)
      'done
      (begin
        ((car procs))
        (call-each (cdr procs)))))
