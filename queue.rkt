#lang racket

(provide
  make-queue
  empty-queue?
  front-queue
  insert-queue!
  delete-queue!
  print-queue)

(define (front-ptr queue) (mcar queue))

(define (rear-ptr queue) (mcdr queue))

(define (set-front-ptr! queue item) (set-mcar! queue item))

(define (set-rear-ptr! queue item) (set-mcdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (mcons '() '()))

(define (front-queue queue)
  (if (not (empty-queue? queue))
      (mcar (front-ptr queue))
      (error "FRONT called on empty queue")))

(define (insert-queue! queue item)
  (let ([new-pair (mcons item '())])
    (if (empty-queue? queue)
        (begin
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair)
          queue)
        (begin
          (set-mcdr! (rear-ptr queue) new-pair)
          (set-rear-ptr! queue new-pair)
          queue))))

(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "DELETE! called on empty queue")
      (begin
        (set-front-ptr! queue (mcdr (front-ptr queue)))
        queue)))

(define (print-queue queue)
  (define (iter ptr)
    (if (not (eq? ptr (rear-ptr queue)))
        (begin
          (printf "~v " (mcar ptr))
          (iter (mcdr ptr)))
        (print (mcar ptr))))
  (if (not empty-queue? queue)
      (begin
        (display "(")
        (iter (front-ptr queue))
        (displayln ")"))
      (displayln "()")))
