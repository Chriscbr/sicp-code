#lang sicp
(#%require r5rs/init)

; prereq functions

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval (+ low 1) high))))

; prime pairs problem

(define (square n) (* n n))

(define (divides? a b) (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (define (next n)
    (cond ((= n 2) 3)
          (else (+ n 2))))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) 
            (list i j))
          (enumerate-interval 
           1 
           (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (unique-pairs n))))

(define (permutations s)
  (if (null? s)   ; empty set?
      (list nil)  ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) 
                        (cons x p))
                      (permutations 
                       (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (equals-sum-triple? s triple)
  (= (+ (car triple)
        (cadr triple)
        (caddr triple)) s))

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j) 
                (map (lambda (k)
                       (list i j k))
                     (enumerate-interval 1 n)))
              (enumerate-interval 1 n)))
   (enumerate-interval 1 n)))

(define (sum-triples n s)
  (filter (lambda (triple)
            (equals-sum-triple? s triple))
          (unique-triples n)))