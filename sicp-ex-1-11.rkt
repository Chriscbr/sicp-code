#lang sicp

(define (f-rec n)
  (cond ((< n 3) n)
        (else (+ (f-rec (- n 1))
                 (* 2 (f-rec (- n 2)))
                 (* 3 (f-rec (- n 3)))))))

(define (f-iter n)
  (define (loop a b c count) ; keeps track of f(n-3), f(n-2) f(n-1), and f(n)
    (cond ((= count 0) a)
          (else (loop b
                      c
                      (+ (* 3 a)
                         (* 2 b)
                         c)
                      (- count 1)))))
  (loop 0 1 2 n))