#lang racket

(define (count-pairs x)
  (define seen '())
  (define (iter x)
    (if (not (mpair? x))
        0
        (if (not (memq x seen))
            (begin
              (set! seen (cons x seen))
              (+ (iter (mcar x))
                 (iter (mcdr x))
                 1))
            0)))
  (iter x))
