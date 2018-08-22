#lang sicp
(#%require r5rs/init)

; Goals:
; 0) List structures: list, booleans, quote, null?, car, cdr
; 1) Basic functions: +, -, *, -, define
; 2) Conditionals: if/cond
; 3) Functions: lambda
; 4) Lambda Calculus









; (+ 8 10) ; 18

; (- 1000 334) ; 666

; (* 2 3 5) ; 30

; (+ 2.7 10) ; 12.7

; (define pi 3.14159)
; (define radius 10)

; (* pi (* radius radius)) ; 314.159









; Conditionals are typically done as
; a case analysis
;(define (abs x)
;  (cond ((> x 0) x)
;        ((= x 0) 0)
;        ((< x 0) (- x))))

; can also include an 'else' at the end









; Mathematical problems with recursion
; lend themselves easily to LISP syntax
(define (factorial n)
  (cond ((= n 0) 1)
        (else
         (* n
            (factorial (- n 1))))))








; (factorial 4)
; (* 4 (factorial 3))
; (* 4 (* 3 (factorial 2)))
; (* 4 (* 3 (* 2 (factorial 1))))
; (* 4 (* 3 (* 2 (* 1 (factorial 0)))))
; (* 4 (* 3 (* 2 (* 1 1)))))
; (* 4 (* 3 (* 2 1)))
; ...
; (* 4 6)
; 24


; Implementations can take advantage of
; tail recursion
(define (fact n k)
  (cond ((= n 1) k)
        (else
         (fact (- n 1) (* k n)))))

; (fact 4 1)
; (fact 3 4)
; (fact 2 12)
; (fact 1 24)
; 24