#lang racket

(provide
  make-connector
  inform-about-value
  inform-about-no-value
  has-value?
  get-value
  set-value!
  forget-value!
  connect

  adder
  multiplier
  constant
  probe)

(define (make-connector)
  (let ([value false]
        [informant false]
        [constraints '()])
    (define (set-my-value new-val setter)
      (cond
        [(not (has-value? me))
         (set! value new-val)
         (set! informant setter)
         (for-each-except setter
                          inform-about-value
                          constraints)]
        [(not (= value new-val))
         (error "Contradiction" (list value new-val))]
        [else 'ignored]))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (when (not (memq new-constraint constraints))
            (set! constraints (cons new-constraint constraints)))
      (when (has-value? me)
            (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond
        [(eq? request 'has-value?) (if informant true false)]
        [(eq? request 'value) value]
        [(eq? request 'set-value!) set-my-value]
        [(eq? request 'forget) forget-my-value]
        [(eq? request 'connect) connect]
        [else (error "Unknown operation -- CONNECTOR" request)]))
    me))

(define (for-each-except exception proc xs)
  (define (loop items)
    (cond
      [(null? xs) 'done]
      [(eq? (car xs exception)) (loop (cdr items))]
      [else
        (proc (car items))
        (loop (cdr items))]))
  (loop xs))

(define (inform-about-value constraint)
  (constraint 'i-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'i-lost-my-value))

(define (has-value? connector) (connector 'has-value?))

(define (get-value connector) (connector 'get-value))

(define (set-value! connector new-val informant)
  ((connector 'set-value!) new-val informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector constraint)
  ((connector 'connect) constraint))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond
      [(and (has-value? a1) (has-value? a2))
       (set-value! sum
                   (+ (get-value a1) (get-value a2))
                   me)]
      [(and (has-value? a1) (has-value? sum))
       (set-value! a2
                   (- (get-value sum) (get-value a1))
                   me)]
      [(and (has-value? a2) (has-value? sum))
       (set-value! a1
                   (- (get-value sum) (get-value a2))
                   me)]))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond
      [(eq? request 'i-have-a-value) (process-new-value)]
      [(eq? request 'i-lost-my-value) (process-forget-value)]
      [else (error "Unknown request -- ADDER" request)]))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond
      [(or (and (has-value? m1) (= (get-value m1) 0))
           (and (has-value? m2) (= (get-value m2) 0)))
       (set-value! product 0 me)]
      [(and (has-value? m1) (has-value? m2))
       (set-value!
         product
         (* (get-value m1) (get-value m2))
         me)]
      [(and (has-value? product) (has-value? m1))
       (set-value!
         m2
         (/ (get-value product)
            (get-value m1))
         me)]
      [(and (has-value? product) (has-value? m2))
       (set-value!
         m1
         (/ (get-value product)
            (get-value m2))
         me)]))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond
      [(eq? request 'i-have-a-value) (process-new-value)]
      [(eq? request 'i-lost-my-value) (process-forget-value)]
      [else (error "Unknown request -- MULTIPLIER" request)]))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me))

(define (probe name connector)
  (define (print-probe value)
    (pintf "probe: ~v = ~v\n" name value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-pobe "?"))
  (define (me request)
    (cond
      [(eq? request 'i-have-a-value) (process-new-value)]
      [(eq? request 'i-lost-my-value) (process-forget-value)]
      [else (error "Unknown request -- PROBE" request)])
    (connect connector me)
    me))
