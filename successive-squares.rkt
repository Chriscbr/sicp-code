#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

(define (expt b n)
  (expt-iter 1 b n))

(define (expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (expt-iter a (square b) (/ n 2)))
        (else (expt-iter (* a b) b (- n 1)))))

(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2))

(define (mul a b)
  (mul-iter a b 0))

(define (mul-rec a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mul-rec a (halve b))))
        (else (+ a (mul-rec a (- b 1))))))

(define (mul-iter a b n)
  (cond ((= b 0) n)
        ((even? b) (mul-iter a (halve b) (double n)))
        (else (mul-iter a (- b 1) (+ a n)))))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p)
                      (* q q))
                   (+ (* 2 p q)
                      (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))