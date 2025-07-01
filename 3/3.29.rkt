#lang racket

(require "../digital_circuit.rkt")

; a || b == ~(~a && ~b)
; a1 = a2 = 0
; c = inv-delay + and-delay (1 inv-delay, т.к. параллельное подключение)
; output = c + inv-delay
; output = 2*inv-delay + and-delay
(define (or-gate a1 a2 output)
  (let ([b1 (make-wire)]
        [b2 (make-wire)]
        [c (make-wire)])
    (inverter
      (and-gate
        (inverter a1 b1)
        (inverter a2 b2)
        c)
      output)))
