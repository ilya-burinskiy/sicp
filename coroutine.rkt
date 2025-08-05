#lang racket

(require "queue.rkt")

(provide
  make-coro
  coro-yield
  exec-coros)

(define *queue* (make-queue))
(define *cc* null)

(define (coro-yield) (call/cc (lambda (k) (*cc* k))))
(define (exec-coros . coros)
  (define (insert-all! coros)
    (when (not (null? coros))
      (begin
        (insert-queue! *queue* (car coros))
        (insert-all! (cdr coros)))))
  (insert-all! coros)
  (run-coros))

(define (run-coros)
  (when (not (empty-queue? *queue*))
    (let* ([coro (front-queue *queue*)]
           [res (call/cc
                 (lambda (k)
                   (begin
                     (set! *cc* k)
                     (coro))))])
      (delete-queue! *queue*)
      (when (procedure? res)
          (insert-queue! *queue* res))
      (run-coros))))

(define-syntax make-coro
  (syntax-rules ()
    [(_) (lambda () (*cc* 'done))]
    [(_ (es ...))
     (lambda () es ... (*cc* 'done))]))
