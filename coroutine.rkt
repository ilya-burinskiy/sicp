#lang racket

(define *queue* null)
(define *cc* null)

(define (coro1)
  (writeln "coro1")
  (yield)
  (writeln "coro1 again")
  (coro-end))

(define (coro2)
  (writeln "coro2")
  (yield)
  (writeln "coro2 again")
  (coro-end))

(define (coro3)
  (writeln "coro3")
  (yield)
  (yield)
  (writeln "coro3 again")
  (coro-end))

(define (enqueue! coro)
  (set! *queue* (cons coro *queue*)))

(define (coro-end) (*cc* 'done))

(define (yield)
  (call/cc (lambda (k) (*cc* k))))

(define (run-coros)
  (when (not (empty? *queue*))
    (let* ([coro (car *queue*)]
           [res (call/cc
                 (lambda (k)
                   (begin
                     (set! *cc* k)
                     (coro))))])
      (if (procedure? res)
          (set!
            *queue*
            (append
              (cdr *queue*)
              (list (lambda () (res null)))))
          (set! *queue* (cdr *queue*)))
      (run-coros))))

(enqueue! coro2)
(enqueue! coro3)
(enqueue! coro1)
(run-coros)
