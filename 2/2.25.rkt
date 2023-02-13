#lang racket

(define list1 (list 1 3 (list 5 7) 9))
(define list2 (list (list 7)))
(define list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (cdr list1)))))
(car (car list2))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3))))))))))))

; list1 = (cons 1 (cons 3 (cons (cons 5 (cons 7 null)) (cons 9 null))))
; (cdr list1) = (cons 3 (cons (cons 5 (cons 7 null)) (cons 9 null)))
; (cdr (cdr list1)) = (cons (cons 5 (cons 7 null)) (cons 9 null))
; (car (cdr (cdr list1))) = (cons 5 (cons 7 null))
; (cdr (car (cdr (cdr list1)))) = (cons 7 null)
; (car (cdr (car (cdr (cdr list1))))) = 7

; list2 = (cons (cons 7 null) null)
; (car list2) = (cons 7 null)
; (car (car list2)) = 7

; list3 = (cons 1 (cons (cons 2 (cons (cons 3 (cons (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 null)) null)) null)) null)) null)) null))
; (cdr list3) = (cons (cons 2 (cons (cons 3 (cons (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 null)) null)) null)) null)) null)) null)
; (car (cdr list3)) = (cons 2 (cons (cons 3 (cons (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 null)) null)) null)) null)) null))
; (cdr (car (cdr list3))) = (cons (cons 3 (cons (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 null)) null)) null)) null)) null)
; (car (cdr (car (cdr list3)))) = (cons 3 (cons (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 null)) null)) null)) null))
; (cdr (car (cdr (car (cdr list3))))) = (cons (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 null)) null)) null)) null)
; (car (cdr (car (cdr (car (cdr list3)))))) = (cons 4 (cons (cons 5 (cons (cons 6 (cons 7 null)) null)) null))
; (cdr (car (cdr (car (cdr (car (cdr list3))))))) = (cons (cons 5 (cons (cons 6 (cons 7 null)) null)) null)
; (car (cdr (car (cdr (car (cdr (car (cdr list3)))))))) = (cons 5 (cons (cons 6 (cons 7 null)) null))
; (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3))))))))) = (cons (cons 6 (cons 7 null)) null)
; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3)))))))))) = (cons 6 (cons 7 null))
; (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3))))))))))) = (cons 7 null)
; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list3)))))))))))) = (cons 7 null)

; (cons 1 (cons (...) null))
; = (cons 1 (cons (cons 2 (cons (...) null)) null))
; = (cons 1 (cons (cons 2 (cons (cons 3 (cons (...) null)) null)) null))
; = (cons 1 (cons (cons 2 (cons (cons 3 (cons (cons 4 (cons (...) null)) null)) null)) null))
; = (cons 1 (cons (cons 2 (cons (cons 3 (cons (cons 4 (cons (cons 5 (cons (...) null)) null)) null)) null)) null))
; = (cons 1 (cons (cons 2 (cons (cons 3 (cons (cons 4 (cons (cons 5 (cons (cons 6 7) null)) null)) null)) null)) null))
