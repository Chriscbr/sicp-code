#lang sicp

(define (pascal row col)
  (cond ((or (= col 1) (= col row)) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))

(define (pascal-nth n)
  (define (loop row col count)
    (cond ((= count 1) (pascal row col))
          (else (if (= row col)
                    (loop (+ row 1) 1 (- count 1))
                    (loop row (+ col 1) (- count 1))))))
  (loop 1 1 n))