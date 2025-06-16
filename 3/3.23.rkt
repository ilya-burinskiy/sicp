#lang racket

(provide make-deque)

; (define (make-qnode val)
;   (let ([val val]
;         [prev '()]
;         [next '()])
;     (define (set-val! new-val) (set! val new-val))
;     (define (set-prev! new-val) (set! prev new-val))
;     (define (set-next! new-val) (set! next new-val))
;     (define (dispatch m)
;       (cond
;         [(eq? m 'set-val!) set-val!]
;         [(eq? m 'set-prev!) set-prev!]
;         [(eq? m 'set-next!) set-next!]
;         [(eq? m 'get-val) val]
;         [(eq? m 'get-prev) prev]
;         [(eq? m 'get-next) next]
;         [else (error "Unknown method -- MAKE-QNODE")]))
;     dispatch))

(define (make-deque)
  (let ([front-ptr '()]
        [rear-ptr '()])
    (define (make-qnode val)
      (let ([val val]
            [prev '()]
            [next '()])
        (define (set-val! new-val) (set! val new-val))
        (define (set-prev! new-val) (set! prev new-val))
        (define (set-next! new-val) (set! next new-val))
        (define (dispatch m)
          (cond
            [(eq? m 'set-val!) set-val!]
            [(eq? m 'set-prev!) set-prev!]
            [(eq? m 'set-next!) set-next!]
            [(eq? m 'get-val) val]
            [(eq? m 'get-prev) prev]
            [(eq? m 'get-next) next]
            [else (error "Unknown method -- MAKE-QNODE")]))
        dispatch))
    (define (set-front-ptr! new-val) (set! front-ptr new-val))
    (define (set-rear-ptr! new-val) (set! rear-ptr new-val))
    (define (empty-deque?) (null? front-ptr))
    (define (front-deque)
      (if (not (empty-deque?))
          (front-ptr 'get-val)
          (error "FRONT called on empty deque")))
    (define (rear-deque)
      (if (not (empty-deque?))
          (rear-ptr 'get-val)
          (error "REAR called on empty deque")))
    (define (front-insert-deque! x)
      (let ([new-node (make-qnode x)])
        (if (not (empty-deque?))
            (begin
              ((new-node 'set-next!) front-ptr)
              ((front-ptr 'set-prev!) new-node)
              (set-front-ptr! new-node)
              dispatch)
            (begin
              (set-front-ptr! new-node)
              (set-rear-ptr! new-node)
              dispatch))))
    (define (rear-insert-deque! x)
      (let ([new-node (make-qnode x)])
        (if (not (empty-deque?))
            (begin
              ((rear-ptr 'set-next!) new-node)
              ((new-node 'set-prev!) rear-ptr)
              (set-rear-ptr! new-node)
              dispatch)
            (begin
              (set-front-ptr! new-node)
              (set-rear-ptr! new-node)
              dispatch))))
    (define (front-delete-deque!)
      (if (not (empty-deque?))
          (begin
            (set-front-ptr! (front-ptr 'get-next))
            dispatch)
          (error "DELETE! called on empty deque")))
    (define (rear-delete-deque!)
      (if (not (empty-deque?))
          (begin
            (set-rear-ptr! (rear-ptr 'get-prev))
            (when (null? rear-ptr) (set-front-ptr! '()))
            dispatch)
          (error "DELETE! called on empty deque")))
    (define (print-deque)
      (define (iter ptr)
        (if (not (eq? ptr rear-ptr))
            (begin
              (printf "~v " (ptr 'get-val))
              (iter (ptr 'get-next)))
            (print (ptr 'get-val))))
      (if (not (empty-deque?))
          (begin
            (display "(")
            (iter front-ptr)
            (displayln ")"))
          (displayln "()")))
    (define (dispatch m)
      (cond
        [(eq? m 'empty-deque?) (empty-deque?)]
        [(eq? m 'front-deque) (front-deque)]
        [(eq? m 'rear-deque) (rear-deque)]
        [(eq? m 'front-insert-deque!) front-insert-deque!]
        [(eq? m 'rear-insert-deque!) rear-insert-deque!]
        [(eq? m 'front-delete-deque!) (front-delete-deque!)]
        [(eq? m 'rear-delete-deque!) (rear-delete-deque!)]
        [(eq? m 'print-deque) (print-deque)]
        [else (error "Unknow method -- MAKE-DEQUE")]))
    dispatch))
