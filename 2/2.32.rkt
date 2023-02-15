#lang racket

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (cons (car s) subset)) rest)))))

; subsets [] = [[]]
; subsets x : xs = let rest = subsets xs
;                  in rest ++ (map (\subset -> x : subset) rest)
; subsets [1, 2, 3] = (subsets [2, 3]) ++ (map (\subset -> 1 : subset) (subsets [2, 3]))
; (subsets [2, 3] = (subsets [3]) ++ (map (\subset -> 2 : subset) (subsets [3]))
; subsets [3] = (subsets []) ++ (map (\subset -> 3 : subset) (subsets []))
; subsets [] = [[]]
; subsets [3] = [[]] ++ (map (\subset -> 3 : subset) [[]])
; subsets [3] = [[], [3]]
; subsets [2, 3] = [[], [3]] ++ (map (\subset -> 2 : subset) [[], [3]])
; subsets [2, 3] = [[], [3], [2], [2, 3]]
; subsets [1, 2, 3] = [[], [3], [2], [2, 3]] ++ (map (\subset -> 1 : subset) [[], [3], [2], [2, 3]])
; subsets [1, 2, 3] = [[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]]
