#lang racket

(require "queue.rkt")

(provide
  fork
  coro-end
  yield
  run-coros)

(define *queue* (make-queue))
(define *cc* null)

(define (fork coro) (insert-queue! *queue* coro))
(define (coro-end) (*cc* 'done))
(define (yield) (call/cc (lambda (k) (*cc* k))))

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
