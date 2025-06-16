#lang racket

(provide make-queue)

(define (make-queue)
  (let ([front-ptr '()]
        [rear-ptr '()])
    (define (set-front-ptr! new-val) (set! front-ptr new-val))
    (define (set-rear-ptr! new-val) (set! rear-ptr new-val))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (not (empty-queue?))
          (mcar front-ptr)
          (error "FRONT called on empty queue")))
    (define (insert-queue! item)
      (let ([new-pair (mcons item '())])
        (if (empty-queue?)
            (begin
              (set-front-ptr! new-pair)
              (set-rear-ptr! new-pair)
              dispatch)
            (begin
              (set-mcdr! rear-ptr new-pair)
              (set-rear-ptr! new-pair)
              dispatch))))
    (define (delete-queue!)
      (if (empty-queue?)
          (error "DELETE! called on empty queue")
          (begin
            (set-front-ptr! (mcdr front-ptr))
            dispatch)))
    (define (print-queue)
      (define (iter ptr)
        (if (not (eq? ptr rear-ptr))
            (begin
              (printf "~v " (mcar ptr))
              (iter (mcdr ptr)))
            (print (mcar ptr))))
      (if (not (empty-queue?))
          (begin
            (display "(")
            (iter front-ptr)
            (displayln ")"))
          (displayln "()")))
    (define (dispatch m)
      (cond
        [(eq? m 'empty-queue?) (empty-queue?)]
        [(eq? m 'front-queue) (front-queue)]
        [(eq? m 'insert-queue!) insert-queue!]
        [(eq? m 'delete-queue!) (delete-queue!)]
        [(eq? m 'print-queue) (print-queue)]
        [else (error "Unknow method -- MAKE-QUEUE" m)]))
    dispatch))
